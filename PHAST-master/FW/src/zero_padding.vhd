--==============================================================================
-- Module Name : ces_phast_zero_padding
-- Library     : ces_phast_lib
-- Project     : PHAST
-- Company     : Campera Electronic Systems Srl
-- Author      : T.Tsiakiris
--------------------------------------------------------------------------------
-- Description: This is a component that inputs a burst of g_burst_depth 16-bit
--              words and outputs the first cam_dos_i of them in a serial manner 
--              while it zeroes the other ones. If the fft_ready_i input is '0',
--              the transmittion is postponed while the fft_tready_i signal is 
--              conveyed to the output.
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
library ces_util_lib;
use ces_util_lib.ces_util_pkg.all;

entity ces_phast_zero_padding is
  generic(
      --max number of subregions
      g_burst_depth : integer := 32;
      g_data_w : integer := 16
    );
  port(
    --system clock
    clk_i: in std_logic;
    --active low reset
    rst_n_i: in std_logic;
    --number of frame subregions (max:32)
    fr_nos_i: in std_logic_vector(f_ceil_log2(g_burst_depth)-1 downto 0);
    --input data (windowing funcion output)
    data_i: in std_logic_vector(g_data_w-1 downto 0);
    --input data valid: asserted for as long as incoming data is valid(coming from windowing
    --component)
    input_dv_i : in std_logic;
    --tlast signal for fft
    fft_tlast_o : out std_logic;
    --outputa data (input of the fft)
    fifo_data_o: out std_logic_vector(g_data_w-1 downto 0);
    --output data valid (going to fft input data valid)
    fifo_wen_o : out std_logic
    );
end entity ces_phast_zero_padding;

architecture zp_arch of ces_phast_zero_padding is

signal s_dont_filtr_cnt : unsigned(f_ceil_log2(g_burst_depth) downto 0);

begin

data_valid_delay_gen : entity ces_util_lib.ces_util_delay
  generic map (
    g_delay  => 1,
    g_data_w => 1
  )
  port map (
    clk_i  => clk_i,
    ce_i   => '1',
    din_i(0)  => input_dv_i,
    dout_o(0) => fifo_wen_o
  );

process(clk_i)
begin

  if rising_edge(clk_i) then
    if input_dv_i='1' then
      s_dont_filtr_cnt <= s_dont_filtr_cnt + 1;
    else
      s_dont_filtr_cnt <= (others => '0');
    end if;

    if s_dont_filtr_cnt=g_burst_depth-1 then
      fft_tlast_o <= '1';
    else
      fft_tlast_o <= '0';
    end if;

    if s_dont_filtr_cnt>unsigned(fr_nos_i) then
      fifo_data_o <= (others => '0');
    else
      fifo_data_o <= data_i;
    end if;

  end if;
end process;


  
end architecture zp_arch;