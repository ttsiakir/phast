--==============================================================================
-- Module Name : ces_phast_mem_int
-- Library     : ces_phast_lib
-- Project     : PHAST
-- Company     : Campera Electronic Systems Srl
-- Author      : T.Tsiakiris
--------------------------------------------------------------------------------
-- Description: This reads 2d matrix data that are stored in a BRAM. The matrix
--              has 32 columns and g_ct_depth/32 lines. Columns and rows are 
--              concatenated in the BRAM. This module unwraps the data by 
--              assigning them with column and row indexes and transmitting each 
--              row to the output as a burst of data. Additionally, a zero padding
--              is applied to the output. All outputs with an index greater than
--              non_fil_fr_i+1 are set to zero.
--              In more detail: The BRAM contains FFT outputs. The number of
--              columns is 32. Equal to the FFT's outputs. Therefore, first column
--              contains the first outputs of the FFT. The second column contains
--              data from the second output etc. With the ces_phast_mem_int
--              module, we wish to output each column as a burst of data. However,
--              sometimes the input FFT frames are less than the BRAM's maximum capacity.
--              The number of input frames we wish to process each time is set by the 
--              input non_fil_fr_i+1. Therefore each output burst is g_ct_depth/32
--              long with only the first non_fil_fr_i+1 passed to the output. The rest
--              of the outputs are set to zero. 
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
library ces_phast_lib;

entity ces_phast_mem_int is
  generic(
      --corner turner depth
      g_ct_depth : integer := 32*1024
    );
  port(
    --system clock
    clk_i: in std_logic;
    --active low reset
    rst_n_i: in std_logic;
    --number of non filtered inputs
    non_fil_fr_i: in std_logic_vector(f_ceil_log2(g_ct_depth/32)-1 downto 0);
    --corner turner uno address
    ct_rd_addr_uno_o : out std_logic_vector(f_ceil_log2(g_ct_depth)-1 downto 0);
    --corner turner uno data
    ct_rd_dat_uno_i : in std_logic_vector(2*16-1 downto 0);
    --corner turner due address
    ct_rd_addr_due_o : out std_logic_vector(f_ceil_log2(g_ct_depth)-1 downto 0);
    --corner turner due data
    ct_rd_dat_due_i : in std_logic_vector(2*16-1 downto 0);
    --corner turner uno read enable
    ct_ren_uno_o : out std_logic;
    --corner turner due read enable
    ct_ren_due_o : out std_logic;
    --corner turner write address that comes from the fft_output_manager and is common to both 
    --corner turners
    ct_wr_addr_i : in std_logic_vector(f_ceil_log2(g_ct_depth) - 1 downto 0);
    --input that indicates what corner turner is currnetly being written and therefore what 
    --corner turner to read from (this input comes from the fft output manager)
    ct_wen_i : in std_logic_vector(2-1 downto 0);
    --fft ready for processing data signal
    fft_ready_i : in std_logic;
    fft_tlast_o : out std_logic;
    --this output is data that is coming from corner turners and is conveyed to the output
    win_dat_o : out std_logic_vector(2*16-1 downto 0);
    --output data valid signal
    win_dv_o : out std_logic
    );
end entity ces_phast_mem_int;

architecture int_arch of ces_phast_mem_int is

--fsm that handles the corner turner memory read
type t_mi_state is (idle, decide, access_col, wait_for_wind, wait_for_fft, access_row);
signal s_mi_reg : t_mi_state := idle;

--this signal is the row index (upper part of corner turner address)
signal s_cnt_row : std_logic_vector(f_ceil_log2(g_ct_depth/32)-1 downto 0);
--this signal is the column index (lower part of cotner turner address)
signal s_cnt_col : std_logic_vector(f_ceil_log2(32)-1 downto 0);
--data valid that is generated by the fsm. Since the ram memory has a delay of two clk cycles after
--asserting read enable, we must delay the data valid signal and then convey it to the output
signal s_fft_valid_before_delay : std_logic;
--output mux select
signal s_out_sel : std_logic_vector(1 downto 0);
signal s_out_sel2 : std_logic_vector(1 downto 0);
signal s_win_cnt : unsigned(1 downto 0);

signal s_win_o : std_logic_vector(2*16-1 downto 0);
signal s_win_dv : std_logic;

begin 

  process(clk_i)
  begin
    if rising_edge(clk_i) then
      if rst_n_i='0' then
        s_cnt_row <= (others => '0');
        s_cnt_col <= (others => '0');
        s_out_sel <= (others => '0');
        s_mi_reg  <= idle;
      else
        --one clock cycle delay for the output data valid signal so that the ram data output can 
        --catch up
        case s_mi_reg is
          when idle => 
            s_cnt_row <= (others => '0');
            s_cnt_col <= (others => '0');
            s_out_sel <= (others => '0');
            s_out_sel2<= (others => '0');
            s_win_cnt <= (others => '0');
            s_fft_valid_before_delay<= '0';
            --start reading the corner turner just before the writing is about to end. This way
            --the current value of ct_wen_i tells us what is the corner turner is (almost) full
            --(and therefore what corner turner to start reading from)
            if (unsigned(ct_wr_addr_i) = unsigned(non_fil_fr_i & "00000")) then
              s_mi_reg <= decide;
            else
              s_mi_reg <= idle;
            end if;
          --the fsm enters this state just before the corner turner writing (carried out by the 
          --fft_outpu_manager module) has ended. If during this time the ct_wen_i signal is 01,
          --this means that the corner turner uno is almost full. If during this time the 
          --ct_wen_i signal is 10, the corner turner due is almost full. 
          when decide => 
            s_fft_valid_before_delay <= '0';
            s_cnt_row <= (others => '0');
            s_cnt_col <= (others => '0');
            if ct_wen_i="00" then
              s_mi_reg <= idle;
            else
              s_mi_reg <= access_row;
              s_out_sel <= ct_wen_i;
              s_out_sel2 <= ct_wen_i;
              s_fft_valid_before_delay <= '1';
            end if;
          when access_row =>
              if s_cnt_row=std_logic_vector(to_unsigned(g_ct_depth/32-1,f_ceil_log2(g_ct_depth/32))) then
                s_cnt_row <= (others => '0');
                s_mi_reg <= wait_for_wind;
                s_fft_valid_before_delay <= '0';
                s_win_cnt <= (others => '0');
              else
                s_cnt_row <= std_logic_vector(unsigned(s_cnt_row) + 1);
                s_mi_reg <= access_row;
                s_fft_valid_before_delay <= '1';
              end if;
          --This a state that allows the windowing component to process data before sending in more data.
          when wait_for_wind => 
            if s_win_cnt=3 then
              s_mi_reg <= wait_for_fft;
              s_win_cnt <= (others => '0');
            else
              s_mi_reg <= wait_for_wind;
              s_win_cnt <= s_win_cnt + 1;
            end if;
          when wait_for_fft =>
            s_fft_valid_before_delay <= '0'; 
            if fft_ready_i='1' then
              s_mi_reg <= access_col;
            else
              s_mi_reg <= wait_for_fft;
            end if;
          when access_col => 
            s_out_sel <= s_out_sel2;
            s_fft_valid_before_delay <= '0';
            --if fft_ready_i='1' then
              if s_cnt_col=std_logic_vector(to_unsigned(32-1,f_ceil_log2(32))) then
                s_cnt_col <= (others => '0');
                s_mi_reg <= idle;
              else
                s_cnt_col<=std_logic_vector(unsigned(s_cnt_col) + 1);
                s_mi_reg <= access_row;
                s_fft_valid_before_delay <= '1';
              end if;
        end case;
      end if;
    end if;
  end process;

  --output data valid must be delayed two clock cycles compared to fsm data valid because of 
  --ram output delay
  ces_util_delay_1 : entity ces_util_lib.ces_util_delay
    generic map (
      g_delay  => 2,
      g_data_w => 1
    )
    port map (
      clk_i  => clk_i,
      ce_i   => '1',
      din_i(0)  => s_fft_valid_before_delay,
      dout_o(0) => s_win_dv
    );  


  --output mux so that fsm controls the output and the ram read enables
  s_win_o   <= ct_rd_dat_uno_i when s_out_sel="01" else
               ct_rd_dat_due_i when s_out_sel="10" else
               (others => '0');
  ct_ren_uno_o <= '1';-- when s_out_sel="01" else '0';
  ct_ren_due_o <= '1';-- when s_out_sel="10" else '0';

  ct_rd_addr_uno_o <= s_cnt_row & s_cnt_col;
  ct_rd_addr_due_o <= s_cnt_row & s_cnt_col;


  mem_int_zp : entity ces_phast_lib.ces_phast_zero_padding
    generic map (
      g_burst_depth => g_ct_depth/32,
      g_data_w => 2*16
    )
    port map (
      clk_i       => clk_i,
      rst_n_i     => rst_n_i,
      fr_nos_i    => non_fil_fr_i,
      data_i      => s_win_o,
      input_dv_i  => s_win_dv,
      fft_tlast_o => fft_tlast_o,
      fifo_data_o => win_dat_o,
      fifo_wen_o  => win_dv_o
    );  
	
end architecture int_arch;