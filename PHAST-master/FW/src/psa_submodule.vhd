--=============================================================================
-- Module Name   : ces_phast_psa_submodule
-- Library       : ces_phast_lib
-- Project       : PHAST
-- Company       : Campera Electronic Systems Srl
-- Author        : T.Tsiakiris
-------------------------------------------------------------------------------
-- Description: This module accepts pnt_nop_i numbers (g_cam_data_w wide each)
--              as input and outputs their average value after some clock cycles.
--              Two data valid signals declare when data is valid at the input 
--              and at the output. The input pnt_nop_i can be from 1 ("000") to 
--              g_max_nop_per_subreg ("111"). pnt_nop_i also determines how many
--              inputs will be used for the average calculation. Therefore if 
--              for examples pnt_nop_i is "010", only the first 3 inputs will 
--              be used and the rest will be dircarded and not used in 
--              calculating the output average.
-------------------------------------------------------------------------------
-- (c) Copyright 2020 Campera Electronic Systems Srl. Via E. Mayer 69, Livorno
-- (Livorno), 57125, Italy. <www.campera-es.com>. All rights reserved.
-- THIS COPYRIGHT NOTICE MUST BE RETAINED AS PART OF THIS FILE AT ALL TIMES.
 
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
 
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
 
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.
--=============================================================================


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library ces_util_lib;
use ces_util_lib.ces_util_pkg.all;


entity ces_phast_psa_submodule is
  generic(
    --frame luminosity data width
    g_cam_data_w : integer := 8;
    --maximum number of pixels(points) per subregion
    g_max_nop_per_subreg : integer := 8;
    --shifting used in the multiplication
    g_mul_shift : integer := 7
  );
  port(
  	--system clock
  	clk_i        : in std_logic;
  	--active low reset
  	rst_n_i      : in std_logic;
    --this input is a concatenation of g_cam_data_i-wide luminosity data that come from a camera
    --output. Since we didnt want to use an array of g_max_nop_per_subreg std_logic_vectors, we 
    --used a single input of g_max_nop_per_subreg data words.
    pnt_lum_i    : in std_logic_vector(g_max_nop_per_subreg*g_cam_data_w-1 downto 0);
    --this input is the number of pixels in the sub-region minus one. 
    --3-bit example: For 1 pixel the input should be 000, for two pixels 001, for three 010 etc.
    pnt_nop_i    : in std_logic_vector(f_ceil_log2(g_max_nop_per_subreg)-1 downto 0);
    --pixel data valid
    pnt_dv_i     : in std_logic;
    --avarage output
    avg_o        : out std_logic_vector(g_cam_data_w-1 downto 0);
    --output data valid
    avg_dv_o     : out std_logic;
    --input average acknowledge
    avg_ack_i    : in  std_logic
  );
end ces_phast_psa_submodule;

-------------------------------------------------------------------------------
-- ARCHITECTURE
-------------------------------------------------------------------------------
architecture rtl of ces_phast_psa_submodule is

constant C_TREE_STRUCT_STAGES  : natural := f_ceil_log2(g_max_nop_per_subreg);

--division look up table functions and signals
type t_mem is array (0 to g_max_nop_per_subreg-1) of unsigned(g_cam_data_w-1 downto 0);

function f_init_mem(k : integer) return t_mem is
  variable v_memory  : t_mem;
begin
  v_memory(0):=(others => '0');
  for i in 1 to k loop
    v_memory(i-1) := to_unsigned(2**g_mul_shift/i,g_cam_data_w);
  end loop;
  return v_memory;
end;

signal s_mem         : t_mem := f_init_mem(g_max_nop_per_subreg);
signal s_avg_shifter : unsigned(g_cam_data_w-1 downto 0);



--signals for calculating mean value

signal s_sum     : unsigned(g_cam_data_w+3-1 downto 0) := (others => '0');

signal s_mult    : std_logic_vector(s_sum'length+g_cam_data_w-1 downto 0) := (others => '0');

--registers for multiplier
signal a         : unsigned(s_sum'length-1 downto 0) := (others => '0');
signal b         : unsigned(g_cam_data_w-1 downto 0);

type std_logic_vector_array is array (integer range g_max_nop_per_subreg downto 0) 
                                              of unsigned(g_cam_data_w+2 downto 0);
signal p: std_logic_vector_array := (others=>(others=>'0'));

signal s_dv_cnt : unsigned(f_ceil_log2(g_max_nop_per_subreg+2)-1 downto 0);
type t_dv_fsm is (idle,raise_dv,wait_ack);
signal s_dv_fsm_reg : t_dv_fsm;

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
	proc_adder_reg : process(clk_i)
	begin
		if rising_edge(clk_i) then
			p((k)) <= unsigned("000" & pnt_lum_i((k)*g_cam_data_w-1 downto (k-1)*g_cam_data_w)) +
  		          p(k-1);
  	end if;
  end process proc_adder_reg;
end generate;

s_sum <=p(to_integer(resize(unsigned(pnt_nop_i),f_ceil_log2(g_max_nop_per_subreg)+1)+1));

--multiplication
	mult_proc_less16: process(clk_i)
	begin
		if rising_edge(clk_i) then
			if rst_n_i='0' then
				s_mult<=(others => '0');
				a <= (others => '0');
				b <= (others => '0');
			else
				a <= s_sum;
				b <= s_avg_shifter;
				s_mult<=std_logic_vector(a * b);
		end if;
	end if;
	end process mult_proc_less16;

dv_fsm_proc : process(clk_i)
begin
	if rising_edge(clk_i) then
		if rst_n_i='0' then
			s_dv_fsm_reg <= idle;
		else
			case(s_dv_fsm_reg) is
				when idle => 
					avg_dv_o <= '0';
					avg_o <= (others => '0');
					s_dv_cnt <= (others => '0');
					if pnt_dv_i='1' then
						s_dv_fsm_reg <= raise_dv;
					else
						s_dv_fsm_reg <= idle;
					end if;
				when raise_dv =>
					if s_dv_cnt=g_max_nop_per_subreg+2-1 then
						s_dv_cnt <= (others => '0');
						s_dv_fsm_reg <= wait_ack;
						avg_o <= s_mult(s_mult'length-g_mul_shift+3-1 downto s_mult'length-g_mul_shift+3-avg_o'length);
					else
						s_dv_fsm_reg <= raise_dv;
						s_dv_cnt <= s_dv_cnt + 1;
						--avg_o <= (others => '0');
					end if;
				when wait_ack => 
					if (avg_ack_i='1') then
						s_dv_fsm_reg <= idle;
						--avg_o <= (others => '0');
						avg_dv_o <= '0';
					else
						s_dv_fsm_reg <= wait_ack;
						--avg_o <= s_mult(s_mult'length-g_mul_shift+3-1 downto s_mult'length-g_mul_shift+3-avg_o'length);
						avg_dv_o <= '1';
					end if;
			end case;
		end if;
	end if;
end process;

end rtl;