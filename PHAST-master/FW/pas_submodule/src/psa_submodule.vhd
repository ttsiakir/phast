library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library ces_util_lib;
use ces_util_lib.ces_util_pkg.all; 

entity psa_submodule is
  generic(
    --frame luminosity data width
    g_cam_data_w : integer := 8;
    --maximum number of pixels(points) per subregion
    g_max_nop_per_subreg : integer := 8
  );
  port(
  	--system clock
  	clk_i        : std_logic;
  	--active low reset
  	rst_i        : std_logic;
    --luminosity data
    pnt_lum_i    : in std_logic_vector(g_max_nop_per_subreg*g_cam_data_w-1 downto 0);
    --this input is the number of pixels in the sub-region minus one. 
    --3-bit example: For 1 pixel the input should be 000, for two pixels 001, for three 010 etc.
    pnt_nop_i    : in std_logic_vector(f_ceil_log2(g_max_nop_per_subreg)-1 downto 0);
    --pixel data valid
    pnt_dv_i     : in std_logic;
    --avarage output
    avg_o        : out std_logic_vector(g_cam_data_w-1 downto 0);
    --output data valid
    avg_dv_o     : out std_logic
  );
end psa_submodule;

architecture rtl of psa_submodule is

constant C_TREE_STRUCT_STAGES  : natural := f_ceil_log2(g_max_nop_per_subreg);

--division look up table functions and signals
type t_mem is array (0 to g_max_nop_per_subreg-1) of unsigned(g_cam_data_w-1 downto 0);

function f_init_mem(k : integer) return t_mem is
  variable v_memory  : t_mem;
begin
  v_memory(0):=(others => '0');
  for i in 1 to k loop
    v_memory(i-1) := to_unsigned(2**7/i,g_cam_data_w);
  end loop;
  return v_memory;
end;

signal s_mem         : t_mem := f_init_mem(g_max_nop_per_subreg);
signal s_avg_shifter : unsigned(g_cam_data_w-1 downto 0);



--signals for calculating mean value

signal s_sum     : unsigned(g_cam_data_w+3-1 downto 0) := (others => '0');
signal s_shifted : unsigned(s_sum'high+7 downto 0) := (others => '0');

signal s_mult    : std_logic_vector(s_shifted'high+s_avg_shifter'high+1 downto 0) := (others => '0');

--registers for multiplier

--signal b2            : unsigned(g_cam_data_w-1 downto 0);
--signal a2        : unsigned(s_sum'high+7 downto 0) := (others => '0');
--signal c2        : std_logic_vector(s_shifted'high+s_avg_shifter'high+1 downto 0) := (others => '0');
signal a         : unsigned(s_sum'high+7 downto 0) := (others => '0');
signal b             : unsigned(g_cam_data_w-1 downto 0);
signal c         : std_logic_vector(s_shifted'high+s_avg_shifter'high+1 downto 0) := (others => '0');

type dv_states is (idle,st1);
signal s_dv_cnt : unsigned(1 downto 0) := (others => '0');
signal s_dv_reg : dv_states := idle;

type std_logic_vector_array is array (integer range g_max_nop_per_subreg downto 0) 
                                              of unsigned(g_cam_data_w+2 downto 0);
signal p: std_logic_vector_array := (others=>(others=>'0'));

begin

  proc_avg: process(clk_i)
  begin
    if rising_edge(clk_i) then
			s_avg_shifter <= s_mem(to_integer(unsigned(pnt_nop_i)));
    end if;
  end process proc_avg;

--dividing by number of pixels--

p(0) <= (others => '0');

--adding signals all inputs
sum_data : for k in 1 to g_max_nop_per_subreg generate
			p((k)) <= unsigned("000" & pnt_lum_i((k)*g_cam_data_w-1 downto (k-1)*g_cam_data_w)) +
  		          p(k-1);
end generate;

adder_reg_proc: process(clk_i)
begin
	if rising_edge(clk_i) then
		if rst_i='0' then
			s_sum <= (others=>'0');
		else
			s_sum <=p(g_max_nop_per_subreg);
		end if;
	end if;
end process adder_reg_proc;

s_shifted <= "0000000" & s_sum;

--multiplication
--gen_bits_less16: if g_cam_data_w < 16 generate
	mult_proc_less16: process(clk_i)
	begin
		if rising_edge(clk_i) then
			if rst_i='0' then
				s_mult<=(others => '0');
				c<=(others => '0');
				a<=(others => '0');
				b<=(others => '0');
			else
				a<=s_shifted;
				b<=s_avg_shifter;
				s_mult<=c;
				c <= std_logic_vector(a * b);
		end if;
	end if;
	end process mult_proc_less16;
--end generate;

--gen_bits_gr16: if g_cam_data_w >= 16 generate
--	mult_proc_gr16: process(clk_i)
--	begin
--		if rising_edge(clk_i) then
--			if rst_i='0' then
--				s_mult<=(others => '0');
--			else
--				a2<=s_shifted;
--				a<=a2;
--				b2<=s_avg_shifter;
--				b <= b2;
--				s_mult<=c2;
--				c2<=c;
--				c <= std_logic_vector(a * b);
--		end if;
--	end if;
--	end process mult_proc_gr16;
--end generate;

--data valid generation
process(clk_i)
begin
	if rising_edge(clk_i) then
		if rst_i='0' then 
			s_dv_cnt <= (others => '0');
			s_dv_reg <= idle;
		  avg_dv_o <= '0';
		  avg_o <= (others => '0');
		else
		  case s_dv_reg is
		  	when idle => 
		  	s_dv_cnt <= (others => '0');
		  	avg_dv_o <= '0';
		  		if pnt_dv_i='1' then
		  			s_dv_reg <= st1;
		  		end if;
		  	when st1 => 
		  		s_dv_cnt <= s_dv_cnt + 1;
		  		avg_dv_o <= '0';
		  	  if s_dv_cnt="11" then
		  			s_dv_reg <= idle;
		  			avg_dv_o <= '1';
		  			avg_o <= s_mult(avg_o'high+7 downto 7);
		  		end if;
		  end case;
	  end if;
	end if;
end process;
--
end rtl;