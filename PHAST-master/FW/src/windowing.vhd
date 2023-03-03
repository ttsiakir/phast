--==============================================================================
-- Module Name : ces_phast_wind
-- Library     : ces_phast_lib
-- Project     : PHAST
-- Company     : Campera Electronic Systems Srl
-- Author      : T.Tsiakiris
--------------------------------------------------------------------------------
-- Description: This is a module that accepts an input burst of fixed width data
--              It outputs another burst whose data is equal to the first burst
--              multiplied by windowing constants. This component prepares data 
--              by applying a windowing function before they are sent to the 
--              Xilinx FFT. Needless to say the constants that are multiplied by
--              with the inputs are fixed point decimals with values from 0 to 1.
--------------------------------------------------------------------------------
-- (c) Copyright 2020 Campera Electronic Systems Srl. Via E. Mayer 69, Livorno
-- (Livorno), 57125, Italy. <www.campera-es.com>. All rights reserved.
-- THIS COPYRIGHT NOTICE MUST BE RETAINED AS PART OF THIS FILE AT ALL TIMES.
--------------------------------------------------------------------------------
 
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
--==============================================================================

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library ces_math_lib;
use ces_math_lib.ces_math_pkg.all;
library ces_util_lib;
use ces_util_lib.ces_util_pkg.all;

entity ces_phast_wind is 
generic (
  --frame input data width
  g_data_w         : integer := 8;
  --max number of subregions
  g_burst_depth       : integer := 32;
  --multiplication output width
  g_win_outp_w        : integer := 16;
  --windowing table width
  g_win_table_w       : integer := 18
  );
port(
  --system clk
  clk_i         : in  std_logic;
  --active low rst
  rst_n_i       : in  std_logic;
  --signals that write the windowing function constants into a ram for later multiplication
  win_w_ena_i   : in  std_logic;
  win_wr_addr_i : in  std_logic_vector(f_ceil_log2(g_burst_depth) - 1 downto 0);
  win_wr_dat_i  : in  std_logic_vector(g_win_table_w - 1 downto 0);
  --averaged luminosity coming from the psa module
  psa_avg_i     : in  std_logic_vector(g_data_w-1 downto 0);
  --data valid signal that stays up for as long as 
  psa_dv_i      : in  std_logic;
  --output data after windowing (concatenated data, number of inputs=number of outputs)
  win_o         : out std_logic_vector(g_win_outp_w-1 downto 0);
  --output data valid (high for as long as output data is valid)
  win_dv_o      : out std_logic
  );
end ces_phast_wind;

architecture wind_arch of ces_phast_wind is

--ram read signals
signal s_win_rd_addr : std_logic_vector(f_ceil_log2(g_burst_depth)-1 downto 0) := (others => '0');
signal s_win_rd_dat : std_logic_vector(g_win_table_w-1 downto 0);
--multiplication full precision output
signal s_mul_out : std_logic_vector(g_win_table_w+g_data_w-1 downto 0);
--counter for counting the multiplier delay and generating the output data valid
signal s_cnt : unsigned(2 downto 0);
--de;ayed ram data to synch with multiplier.
signal s_ram_data_delayed : std_logic_vector(g_win_table_w-1 downto 0);

--buffer for allowing the ram data output catch up with the data input at the multiplier input
--Remainder: It takes two clock cycles for the ram data to appear at the ram output after 
--changing the ram address.
type slv_buffer is array (2 downto 0) of std_logic_vector(g_data_w-1 downto 0);
signal s_psa_dat_buf : slv_buffer;

--an fsm for generating the output data valid as well as managing the ram address
type t_fsm_state is (idle,dv_assert,access_ram,dv_deassert);
signal s_reg : t_fsm_state;

begin


  -- a ram that is used to store the windowing function constants.
  win_ram : entity ces_util_lib.ces_util_ram_r_w
  generic map (
    g_ram_latency => 2,
    g_ram_data_w  => g_win_table_w,
    g_ram_depth   => g_burst_depth,
    g_init_file   => "",
    g_simulation  => 1
  )
  port map (
    clk_i     => clk_i,
    ena_i     => '1',
    wen_i     => win_w_ena_i,
    wr_addr_i => win_wr_addr_i,
    wr_dat_i  => win_wr_dat_i,
    enb_i     => '1',
    rd_addr_i => s_win_rd_addr,
    rd_dat_o  => s_win_rd_dat
  );

  ces_util_delay_1 : entity ces_util_lib.ces_util_delay
  generic map (
    g_delay  => 1,
    g_data_w => g_win_table_w
  )
  port map (
    clk_i  => clk_i,
    ce_i   => '1',
    din_i  => s_win_rd_dat,
    dout_o => s_ram_data_delayed
  );  

  --multiplier to aply the windowing function
  win_multiplier : entity ces_math_lib.ces_math_fi_mult
  generic map (
    g_din_a_w      => g_data_w,
    g_din_a_binpnt => 0,
    g_din_b_w      => g_win_table_w,
    g_din_b_binpnt => g_win_table_w,
    g_dout_w       => g_win_table_w+g_data_w,
    g_dout_binpnt  => g_win_table_w,
    g_round_mode   => C_CES_ROUND,
    g_din_a_type   => C_CES_UNSIGNED,
    g_din_b_type   => C_CES_UNSIGNED,
    g_dout_type    => C_CES_UNSIGNED,
    g_pipe_stages  => 2
  )
  port map (
    clk_i  => clk_i,
    din1_i => s_psa_dat_buf(2),
    din2_i => s_ram_data_delayed,
    dout_o => s_mul_out
  );  

  --fsm for ram address manipulation and output data valid generation
  dv_gen_proc : process(clk_i)
  begin
    if rising_edge(clk_i) then
      if rst_n_i='0' then
        win_dv_o <= '0';
        s_cnt <= (others => '0');
        s_win_rd_addr <= (others => '0');
      else
        case s_reg is 
        --when idle wait for the input data valid that means new data are coming
          when idle => 
            win_dv_o <= '0';
            s_cnt <= (others => '0');
            s_win_rd_addr <= (others => '0');
            if (psa_dv_i='1') then
              s_win_rd_addr <= std_logic_vector(unsigned(s_win_rd_addr)+1);
              s_reg <= dv_assert;
            end if;
          --when input data valid is detected, access the ram and send after multiplier delay,
          --assert output data valid.
          when dv_assert => 
          s_win_rd_addr <= std_logic_vector(unsigned(s_win_rd_addr)+1);
            if s_cnt=4 then 
              win_dv_o <= '1';
              s_reg <= access_ram;
            else
              s_cnt <= s_cnt + 1;
            end if;
          --access the ram until input data valid is zero
          when access_ram => 
            win_dv_o <= '1';
            s_cnt <= (others => '0');
            s_win_rd_addr <= std_logic_vector(unsigned(s_win_rd_addr)+1);
            if (psa_dv_i='0') then
              s_reg <= dv_deassert;
            end if;
          --after a designated delay, deassert output data valid and return to idle.
          when dv_deassert => 
            s_win_rd_addr <= (others => '0');
            if s_cnt=4 then 
              win_dv_o <= '0';
              s_reg <= idle;
            else
              s_cnt <= s_cnt + 1;
            end if;
        end case;
      end if;
    end if;
  end process dv_gen_proc;

  --buffers that delay the input data to let the ram read data catch up
  buff_proc : process(clk_i)
  begin
    if rising_edge(clk_i) then
      if rst_n_i='0' then
        s_psa_dat_buf <= (others => (others => '0'));
      else
        for k in 1 to 2 loop
          s_psa_dat_buf(k) <= s_psa_dat_buf(k-1);
        end loop;
        s_psa_dat_buf(0) <= psa_avg_i;
      end if;
    end if;
  end process;

  win_o <= s_mul_out(g_win_table_w+g_data_w-1 downto g_win_table_w+g_data_w-g_win_outp_w);

end wind_arch;