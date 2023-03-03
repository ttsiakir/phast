--==============================================================================
-- Module Name : ces_phast_fft_out_mng
-- Library     : ces_phast_lib
-- Project     : PHAST
-- Company     : Campera Electronic Systems Srl
-- Author      : T.Tsiakiris
--------------------------------------------------------------------------------
-- Description: 
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

entity ces_phast_fft_out_mng is
  generic(
      --max number of subregions
      g_max_nos : integer := 32;
      g_ram_depth : integer := 32*1024
    );
  port(
    --system clock
    clk_i: in std_logic;
    --active low reset
    rst_n_i: in std_logic;
    --number of frames to be sent to the second fft (max:1024)
    non_fil_fr_i: in std_logic_vector(f_ceil_log2(g_ram_depth/32)-1 downto 0);
    --fft output data coming from axis bus
    axis_data_tdata_i : in std_logic_vector (2*16-1 downto 0);
    --fft axis bus tuser
    axis_data_tuser_i : in std_logic_vector (15 downto 0);
    --axis bus data valid (fft ready to transmit data signal)
    axis_data_tvalid_i : in std_logic;
    --ready to receive data from axis bus
    axis_data_tready_o : out std_logic;
    --signal that signifies this is the last data that is transmitted from the fft.
    axis_data_tlast_i : in std_logic;
    --corner turner wite enable
    ct_wen_o : out std_logic_vector(1 downto 0);
    -- corner turner write address
    ct_wr_addr_o : out std_logic_vector(f_ceil_log2(g_ram_depth) - 1 downto 0);
    --fft data output
    ct_wr_data_o : out std_logic_vector (2*16-1 downto 0)
    );
end entity ces_phast_fft_out_mng;

architecture fft_out_mng_arch of ces_phast_fft_out_mng is

--fsm states
type t_fsm_state is (idle,wen_st,incr_sel);
signal s_reg : t_fsm_state;
--signal that counts what part of ram to write the data to. Reminder: The corner turner ram 
--is 32x1024 words deep which means that you can store 1024 fft outputs. This select signal is
--1-bit more than the width needed to count every corner turner address and this happens 
--because we need to detect overflow to start writing to a second corner turner
signal s_sel : std_logic_vector(f_ceil_log2(g_ram_depth/g_max_nos) downto 0);
--corner turner write enable
signal s_ct_wen : std_logic;

--buffer for fft output index
signal s_index : std_logic_vector(f_ceil_log2(g_max_nos)-1 downto 0);
--buffer for axis bus data
signal s_axis_data :  std_logic_vector (2*16-1 downto 0);

begin

process(clk_i)
begin
if rising_edge(clk_i) then
  if rst_n_i='0' then
    axis_data_tready_o <= '0';
    s_ct_wen <= '0';
    s_sel <= (others => '0');
  else 
    case s_reg is
    --idle state: the unit is ready for receiving data and awaits a valid input from axis bus
      when idle =>
        s_ct_wen <= '0';
        axis_data_tready_o <= '1';
        if (axis_data_tvalid_i='1') then
          s_reg <= wen_st;
          s_ct_wen <= '1';
        end if;
      --this state keeps corner turner write enable high until all data are received
      when wen_st =>
        axis_data_tready_o <= '1';
        if axis_data_tlast_i='1' then
          s_reg <= incr_sel;
          s_ct_wen <= '1';
        else
          s_reg <= wen_st;
        end if;
      --this state increases the s_sel signal before waiting for the next output by the fft.
      when incr_sel => 
        s_reg <= idle;
        if s_sel(f_ceil_log2(g_ram_depth/g_max_nos)-1 downto 0)=non_fil_fr_i then
          s_sel <= (others => '0');
        else
          s_sel <= std_logic_vector(unsigned(s_sel)+1);
        end if;
        s_ct_wen <= '0';
    end case;
  end if;
end if;
end process;

--this process buffers incoming data and indexing numbers. This way there is enough time to 
--sense that the valid signal has gone high and no data is missed.
data_sampl_proc : process(clk_i)
begin
if rising_edge(clk_i) then
  if rst_n_i='0' then
    s_axis_data <= (others => '0');
    s_index <= (others => '0');
  else
    s_axis_data <= axis_data_tdata_i;
    s_index <= axis_data_tuser_i(f_ceil_log2(g_max_nos)-1 downto 0);
  end if;
end if;
end process data_sampl_proc;

--data output that serves as corner turner input.
ct_wr_data_o <= s_axis_data;
ct_wr_addr_o <= s_sel(f_ceil_log2(g_ram_depth/g_max_nos)-1 downto 0) & s_index;
--If ct_wen_o(0) is high then the first corneer turner is selected. If ct_wen_o(1) is high 
--then the second corneer turner is selected.
--How it works:
--The msb of the s_sel signal serves as the select signal for corner turner one and two.
--The rest of the s_sel signal serves as a select signal for one of the 32 areas that the
--corner turner is divided to. (Each time there is a reult from the fft, the 32 outputs are
--stored in the next part of the memory). The concatenated signal 
--s_sel(f_ceil_log2(g_max_nos)-1 downto 0) & axis_data_tuser_i(4 downto 0) helps us find the
--memory address that the result should be stored to.
ct_wen_o(0) <= s_ct_wen when s_sel(f_ceil_log2(g_ram_depth/g_max_nos))='0' else '0';
ct_wen_o(1) <= s_ct_wen when s_sel(f_ceil_log2(g_ram_depth/g_max_nos))='1' else '0';
  
end architecture fft_out_mng_arch;