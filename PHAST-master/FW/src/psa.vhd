--=============================================================================
-- Module Name   : ces_phast_psa
-- Library       : ces_phast_lib
-- Project       : PHAST
-- Company       : Campera Electronic Systems Srl
-- Author        : T.Tsiakiris
-------------------------------------------------------------------------------
-- Description: This module is responsible for picking up pixels coming from 
--              a frame. This is how it works: The user can set the coordinates 
--              of the pixels that interest him. The max number of coordinate 
--              pairs is 8 pairs for 32 areas  of the frame. When the user declares  
--              which pixels he wants to use, the number of subregion to which  
--              these pixels belong must also be declared (max g_max_nop_per_subreg
--              pixels per subregion). The module then scans the frame and calculates  
--              the average luminosity of the indicated pixels for each of 32  
--              subregions. The results are sent to the outut as a burst of 32 
--              averaged luminosities accompanied by a data valid signal.
--
--                         *Important Consderations*
--
--1) One pixel can belong to more than one areas (so called "regions" or "subregions").
--2) The number of pixels corresponding to a subregion is pnt_nop_i+1. Therefore
--   if there are 3 pixels, we must ideclare pnt_nop_i="010" (i.e. 2 instead of 3).
-------------------------------------------------------------------------------
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
--=============================================================================


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library ces_util_lib;
use ces_util_lib.ces_util_pkg.all;	

library ces_phast_lib;


entity ces_phast_psa is
  generic(
    --frame luminosity data width
    g_cam_data_w : integer := 8;
    --number of points per subregion
    g_max_nop_per_subreg : integer := 8;
    --data channles that come from the AXI bus (power of two)
    g_data_channels : integer := 4;
    --maximum camera frame dimensions (default settings : 8k)
    g_v_active_max : integer := 4320;
    g_h_active_max : integer := 7680
  );
  port(
    --system clk
    clk_i        : in std_logic;
    --active low rst
    rst_n_i        : in std_logic;
    --horizontal coordinate for pixel of interest (input form the software-g_max_nop_per_subreg concatenated inputs)
    pnt_h_i      : in std_logic_vector(g_max_nop_per_subreg*f_ceil_log2(g_h_active_max)-1 downto 0);
    --vertical coordinate for pixel of interest (input form the software-g_max_nop_per_subreg concatenated inputs)
    pnt_v_i      : in std_logic_vector(g_max_nop_per_subreg*f_ceil_log2(g_v_active_max)-1 downto 0);
    --the subregion number that the pixels belong to
    pnt_nos_i    : in std_logic_vector(5-1 downto 0);
    --number of pixels in the sub-region
    pnt_nop_i    : in std_logic_vector(32*f_ceil_log2(g_max_nop_per_subreg)-1 downto 0);
    --pixel data valid
    pnt_dv_i     : in std_logic;
    --frame data input
    fr_tdata_i   : in std_logic_vector(g_data_channels*g_cam_data_w-1 downto 0);
    --frames vertical active
    fr_vact_i    : in std_logic;
    --frames horizontal active
    fr_hact_i    : in std_logic;
    --frames data enable
    fr_de_i      : in std_logic;
    --frames vertical sync
    fr_vsync_i   : in std_logic;
    --frames horizontal sync
    fr_hsync_i   : in std_logic;
    --32 concatenated outputs
    avg_o        : out std_logic_vector(g_cam_data_w-1 downto 0);
    --output data valids
    avg_dv_o     : out std_logic
  );
end ces_phast_psa;

architecture rtl_arch of ces_phast_psa is

--signal types used for assigning concatenated input signals to arrays:
--the average outputs
type std_logic_vector_array is array (32-1 downto 0) of std_logic_vector(g_cam_data_w-1 downto 0);
--number of points per subregion
type std_logic_vector_array3 is array (32-1 downto 0) of std_logic_vector(f_ceil_log2(g_max_nop_per_subreg)-1 downto 0);
--vertical coordinates (8-pixel input)
type ver_coor is array (g_max_nop_per_subreg-1 downto 0) of std_logic_vector(f_ceil_log2(g_v_active_max)-1 downto 0);
--horisontal coodinates (8 pixel input)
type hor_coor is array (g_max_nop_per_subreg-1 downto 0) of std_logic_vector(f_ceil_log2(g_h_active_max)-1 downto 0);
--array of the vertical coordnates of every region.
type array_of_h_coor is array (32-1 downto 0) of hor_coor;
--array of the horisontal coordinates of every region.
type array_of_v_coor is array (32-1 downto 0) of ver_coor;
--array to assign submodule inputs
type t_lum_array is array (32-1 downto 0) of std_logic_vector(g_max_nop_per_subreg*g_cam_data_w-1 downto 0);
--luminosity inputs for the psa submodule
type array_of_8_lums is array (g_max_nop_per_subreg-1 downto 0) of std_logic_vector(g_cam_data_w-1 downto 0);
type array_of_array_of_8_lums is array (32-1 downto 0) of array_of_8_lums;


--signal that is used to allocate concatenated data to an std_logic_vector array.
--they are more easily managable this way.

signal s_avg : std_logic_vector_array;
--luminosity inputs for the psa submodule
signal s_lum : array_of_array_of_8_lums := (others => (others => (others => '0')));
signal s_lum_buffer : array_of_array_of_8_lums;
--number of points per subregion
signal s_nop : std_logic_vector_array3 := (others => (others => '0'));

--horisontal coodinates (concatenated input but in a more easily managable way)
signal s_h : hor_coor := (others => (others => '0'));
--vertical coordinates (concatenated input but in a more easily managable way)
signal s_v : ver_coor := (others => (others => '0'));
--array of the horizontal coordnates of every region. (Contains data of ALL the regions)
signal s_h_coor : array_of_h_coor := (others => (others => (others => '0')));
--array of the vertical coordinates of every region. (Contains data of ALL the regions)
signal s_v_coor : array_of_v_coor := (others => (others => (others => '0')));
--signa that will be used as psa_submodule inputs
signal s_pnt_lum : t_lum_array;
--the above signal's data valid
signal s_pnt_dv  : std_logic_vector(31 downto 0) := (others => '0');

--frames RGB data input
--signal hdmi_tdata   : std_logic_vector(g_cam_data_w-1 downto 0);

--counters for frame ram accessing:
--counter for accessing subregions in the fsm
signal s_scnt: unsigned(f_ceil_log2(32)-1 downto 0);
signal s_pcnt_adv : std_logic_vector(32-1 downto 0);
--counter of frame columns
signal s_hcnt: unsigned(f_ceil_log2(g_h_active_max)-f_ceil_log2(g_data_channels)-1 downto 0):= (others => '0');
signal s_hcnt_next : unsigned(f_ceil_log2(g_h_active_max)-f_ceil_log2(g_data_channels)-1 downto 0):= (others => '0');
--counter of frame rows
signal s_vcnt: unsigned(f_ceil_log2(g_v_active_max)-1 downto 0) := to_unsigned(1,f_ceil_log2(g_v_active_max));


type t_buffer is array (g_data_channels-1 downto 0) of std_logic_vector(g_cam_data_w-1 downto 0);
type t_buffer_array is array(32-1 downto 0) of t_buffer;
signal s_tdata  : t_buffer;
--mux for picking channel data and store it to s_lum
signal s_ch_pick : array_of_array_of_8_lums;
signal s_tdata_delay : std_logic_vector(g_data_channels*g_cam_data_w-1 downto 0);
--pixel coordinates match signal
type t_pnt_match is array (32-1 downto 0) of std_logic_vector(g_max_nop_per_subreg-1 downto 0);
signal s_pnt_match : t_pnt_match;
signal s_pnt_match_rise : t_pnt_match := (others => (others => '0'));
type t_pnt_inc is array(32-1 downto 0) of std_logic_vector(f_ceil_log2(g_data_channels)-1 downto 0);
--s_pcnt incrementor
signal s_pnt_inc : t_pnt_inc := (others => (others => '0'));

--the fsm states for grabbing frame data.
type t_fr_state is (idle,st1,st2);
type t_fr_state_array is array(31 downto 0) of t_fr_state;
signal s_fr_reg : t_fr_state_array := (others => idle);

--ram signals
signal s_wr_addr : std_logic_vector(f_ceil_log2(128*64) - 1 downto 0);
signal s_rd_addr : std_logic_vector(f_ceil_log2(g_h_active_max*g_v_active_max) - 1 downto 0);
signal s_rd_dat  : std_logic_vector(g_cam_data_w - 1 downto 0);
--This signal is used to split the ram data to log2(64) regions. Explanation: The
--ram contains frames coming from the camera. The user specifies the pixels that
--are important by providing the vertical and horizontal coordinates of the pixels
--(inputs pnt_h_i and pnt_v_i). However the data in the ram are written without
--indication as to where a new frame line begins. Therefore we need to divide the
--ram data into log2(64) slices (remember this is a 128x64 camera). To do this, we
--are using this s_v_sel signal. By left shifting log2(128) times, we get the 
--vertical frame coordinate
signal s_v_sel : std_logic_vector(f_ceil_log2(g_v_active_max)-1 downto 0) := (others => '0');
--horizontal frame coordinate
signal s_h_sel : std_logic_vector(f_ceil_log2(g_h_active_max)-1 downto 0) := (others => '0');
--pixel coordinates upper part
alias s_hsel_high : std_logic_vector(f_ceil_log2(g_h_active_max)-f_ceil_log2(g_data_channels)-1 downto 0) is s_h_sel(f_ceil_log2(g_h_active_max)-1 downto f_ceil_log2(g_data_channels));
--channel index
alias s_ch_index : std_logic_vector(f_ceil_log2(g_data_channels)-1 downto 0) is s_h_sel(f_ceil_log2(g_data_channels)-1 downto 0);
--psa submodules data valid
signal s_avg_dv : std_logic_vector(32-1 downto 0);
signal s_avg_dv_or : std_logic;
signal s_avg_ack : std_logic_vector(32-1 downto 0);
--signal that notifyies which of the data valids have been raised
signal s_avg_dv_rise : std_logic_vector(32-1 downto 0);
--decoder output. This decoder is used to generate the ram address.
signal s_enc : std_logic_vector(f_ceil_log2(32)-1 downto 0);

--ram that collects average luminosites before sending them all to the fft as a burst
signal s_b_wen     : std_logic;
signal s_b_wr_dat  : std_logic_vector(g_cam_data_w - 1 downto 0);
signal s_b_wr_addr : std_logic_vector(f_ceil_log2(32) - 1 downto 0);
signal s_b_rd_addr : std_logic_vector(f_ceil_log2(32) - 1 downto 0) := (others => '0');
--signal s_b_rd_dat  : std_logic_vector(g_cam_data_w - 1 downto 0);

--fsm for conveying all the data at the output as a burst when the frame is over.
type t_brst_state is (idle, store_and_ack, wait_one_clk, burst, wait_for_frame);
signal s_brst_reg : t_brst_state := idle;
signal s_brst_cnt : unsigned(f_ceil_log2(32)-1 downto 0);

--for positive edge detector of fr_vsync_i
--signal s_fr_vsync : std_logic;
--signal s_fr_vsync_rising_edge : std_logic;

--signal that delays the output data valid so that the avg_ram output can catch up
signal s_out_dv_buff : std_logic := '0';

signal s_hcnt_rst : std_logic;
signal s_vcnt_rst : std_logic;
signal vtemp : std_logic;
signal htemp : std_logic;

begin
  
  --s_avg is a list that contains all the outputs of the submodules. More specifically,
  --it contains the averaged luminosities of every subregion of the image (therefore it is
  --a list of 8 elements). We assign those luminosities to the concatenated output.
  --avg_output_gen: for k in 0 to 31 generate
  --    avg_o((k+1)*(g_cam_data_w)-1 downto k*(g_cam_data_w)) <= s_avg(k);
  --end generate;
  
  --concatenated luminosities
  lum_input_gen : for m in 0 to 31 generate
    subcoor_gen : for k in 0 to g_max_nop_per_subreg-1 generate
      s_pnt_lum(m)((k+1)*g_cam_data_w-1 downto k*g_cam_data_w) <= s_lum(m)(k);
    end generate;
  end generate;

  --concatenated coordinates
  coor_input_gen: for k in 0 to g_max_nop_per_subreg-1 generate
      s_h(k) <= pnt_h_i((k+1)*f_ceil_log2(g_h_active_max)-1 downto k*f_ceil_log2(g_h_active_max));
      s_v(k) <= pnt_v_i((k+1)*f_ceil_log2(g_v_active_max)-1 downto k*f_ceil_log2(g_v_active_max));
  end generate;
  
  nop_input_gen: for k in 0 to 31 generate
      s_nop(k) <= pnt_nop_i((k+1)*f_ceil_log2(g_max_nop_per_subreg)-1 downto k*f_ceil_log2(g_max_nop_per_subreg));
  end generate;
  
  --when data valid is high, the process below saves 8 coordinate pairs (s_h and s_v)
  --to the s_h_coor and s_v_coor arrays which are basically two dimensional arrays of
  --log2(128) and log2(64) data. (Remember this is a 128x64 camera).
  --In more detail: s_v_coor and s_h_coor are two dimensional arrays that contain ALL the
  --coordinates form ALL 32 regions that the user wants them to be sampled. Every time the 
  --data valid goes high, 8 coordinate parts that belong to the region specified by the 
  --pnt_nos_i input are updated.
  --Example: The coordinates of the 4th pixel that belongs to the 14th subregion are:
  --Horizontal: s_h_coor(13)(3)
  --Vertical: s_v_coor(13)(3)
coor_proc: process(clk_i)
  begin
    if (rising_edge(clk_i)) then
      if pnt_dv_i='1' then
        if    pnt_nos_i="00000" then
          s_h_coor(0) <= s_h;
          s_v_coor(0) <= s_v;
        elsif pnt_nos_i="00001" then
          s_h_coor(1) <= s_h;
          s_v_coor(1) <= s_v;
        elsif pnt_nos_i="00010" then
          s_h_coor(2) <= s_h;
          s_v_coor(2) <= s_v;
        elsif pnt_nos_i="00011" then
          s_h_coor(3) <= s_h;
          s_v_coor(3) <= s_v;
        elsif pnt_nos_i="00100" then
          s_h_coor(4) <= s_h;
          s_v_coor(4) <= s_v;
        elsif pnt_nos_i="00101" then
          s_h_coor(5) <= s_h;
          s_v_coor(5) <= s_v;
        elsif pnt_nos_i="00110" then
          s_h_coor(6) <= s_h;
          s_v_coor(6) <= s_v;
        elsif pnt_nos_i="00111" then
          s_h_coor(7) <= s_h;
          s_v_coor(7) <= s_v;
        elsif pnt_nos_i="01000" then
          s_h_coor(8) <= s_h;
          s_v_coor(8) <= s_v;
        elsif pnt_nos_i="01001" then
          s_h_coor(9) <= s_h;
          s_v_coor(9) <= s_v;
        elsif pnt_nos_i="01010" then
          s_h_coor(10) <= s_h;
          s_v_coor(10) <= s_v;
        elsif pnt_nos_i="01011" then
          s_h_coor(11) <= s_h;
          s_v_coor(11) <= s_v;
        elsif pnt_nos_i="01100" then
          s_h_coor(12) <= s_h;
          s_v_coor(12) <= s_v;
        elsif pnt_nos_i="01101" then
          s_h_coor(13) <= s_h;
          s_v_coor(13) <= s_v;
        elsif pnt_nos_i="01110" then
          s_h_coor(14) <= s_h;
          s_v_coor(14) <= s_v;
        elsif pnt_nos_i="01111" then
          s_h_coor(15) <= s_h;
          s_v_coor(15) <= s_v;
        elsif pnt_nos_i="10000" then
          s_h_coor(16) <= s_h;
          s_v_coor(16) <= s_v;
        elsif pnt_nos_i="10001" then
          s_h_coor(17) <= s_h;
          s_v_coor(17) <= s_v;
        elsif pnt_nos_i="10010" then
          s_h_coor(18) <= s_h;
          s_v_coor(18) <= s_v;
        elsif pnt_nos_i="10011" then
          s_h_coor(19) <= s_h;
          s_v_coor(19) <= s_v;
        elsif pnt_nos_i="10100" then
          s_h_coor(20) <= s_h;
          s_v_coor(20) <= s_v;
        elsif pnt_nos_i="10101" then
          s_h_coor(21) <= s_h;
          s_v_coor(21) <= s_v;
        elsif pnt_nos_i="10110" then
          s_h_coor(22) <= s_h;
          s_v_coor(22) <= s_v;
        elsif pnt_nos_i="10111" then
          s_h_coor(23) <= s_h;
          s_v_coor(23) <= s_v;
        elsif pnt_nos_i="11000" then
          s_h_coor(24) <= s_h;
          s_v_coor(24) <= s_v;
        elsif pnt_nos_i="11001" then
          s_h_coor(25) <= s_h;
          s_v_coor(25) <= s_v;
        elsif pnt_nos_i="11010" then
          s_h_coor(26) <= s_h;
          s_v_coor(26) <= s_v;
        elsif pnt_nos_i="11011" then
          s_h_coor(27) <= s_h;
          s_v_coor(27) <= s_v;
        elsif pnt_nos_i="11100" then
          s_h_coor(28) <= s_h;
          s_v_coor(28) <= s_v;
        elsif pnt_nos_i="11101" then
          s_h_coor(29) <= s_h;
          s_v_coor(29) <= s_v;
        elsif pnt_nos_i="11110" then
          s_h_coor(30) <= s_h;
          s_v_coor(30) <= s_v;
        elsif pnt_nos_i="11111" then
          s_h_coor(31) <= s_h;
          s_v_coor(31) <= s_v;
        end if;
      end if;
    end if;
  end process coor_proc;

  
  --pas submodules: input is luminosity and number of points and output is average
  --luminosity.
  psa_submodule_gen: for m in 0 to 31 generate
    psa_submodule : entity ces_phast_lib.ces_phast_psa_submodule
      generic map (
        g_cam_data_w         => 8,
        g_max_nop_per_subreg => g_max_nop_per_subreg,
        g_mul_shift => 7
      )
      port map (
        clk_i     => clk_i,
        rst_n_i   => rst_n_i,
        pnt_lum_i => s_pnt_lum(m),
        pnt_nop_i => s_nop(m),
        pnt_dv_i  => s_pnt_dv(m),
        avg_o     => s_avg(m),
        avg_dv_o  => s_avg_dv(m),
        avg_ack_i => s_avg_ack(m)
      );  
  end generate;

--------------------------------------------------------------------------------
---FSMs for picking up the right pixels and sending them to the psa_submodules--
--------------------------------------------------------------------------------

  pck_pixels_gen : for m in 0 to 31 generate
  
      
      subregion_pixel_gen : for n in 0 to g_max_nop_per_subreg-1 generate
        s_pnt_match(m)(n) <= '1' when s_h_coor(m)(n)(f_ceil_log2(g_h_active_max)-1 downto f_ceil_log2(g_data_channels)) = std_logic_vector(s_hcnt) and std_logic_vector(s_vcnt) = s_v_coor(m)(n) else
                     '0';

        s_ch_pick(m)(n) <= s_tdata(to_integer(unsigned(s_h_coor(m)(n)(f_ceil_log2(g_data_channels)-1 downto 0))));

        process(clk_i)
      begin

        if rising_edge(clk_i) then
          if s_pnt_match(m)(n)='1' then
            s_lum(m)(n) <= s_ch_pick(m)(n);
          end if;
        end if;
      end process;

      end generate subregion_pixel_gen;
    
      process(clk_i)
      begin

        if rising_edge(clk_i) then
          for k in 0 to g_max_nop_per_subreg-1 loop
            if (s_vcnt_rst='1') then
              s_pnt_match_rise(m)(k) <= '0';
            else
              if (s_pnt_match(m)(k)='1') then
                s_pnt_match_rise(m)(k) <= '1';
              end if;
            end if;
          end loop;
        end if;
      end process;
    
    psa_sub_input_dv_gen : entity ces_util_lib.ces_util_edge_detector
      generic map (
        g_event_edge => C_RISING_EDGE
      )
      port map (
        clk_i  => clk_i,
        din_i  => f_vector_and(s_pnt_match_rise(m)),
        dout_o => s_pnt_dv(m)
      );      
  
      s_pcnt_adv(m) <= f_vector_or(s_pnt_match(m));
      end generate;

  s_vcnt_rst <= vtemp and (not fr_vact_i);
  s_hcnt_rst <= htemp and (not fr_hact_i);

  s_vcnt_incrementor : process(clk_i)
  begin
    if rising_edge(clk_i) then
      vtemp <= fr_vact_i;
      htemp <= fr_hact_i;
      if rst_n_i='0' then 
        s_vcnt <= (others => '0');
      elsif s_vcnt_rst='1' then
        s_vcnt <= (others => '0');
      elsif s_hcnt_rst='1' then
        s_vcnt <= s_vcnt + 1;
      else
        s_vcnt <= s_vcnt;
      end if;
    end if;
  end process s_vcnt_incrementor;

  s_hcnt_incrementor : process(clk_i)
  begin
    if rising_edge(clk_i) then
      vtemp <= fr_vact_i;
      htemp <= fr_hact_i;
      if rst_n_i='0' then 
        s_hcnt <= (others => '0');
      elsif s_hcnt_rst='1' then
        s_hcnt <= (others => '0');
      else
        s_hcnt <= s_hcnt_next;
      end if;
    end if;
  end process s_hcnt_incrementor;

  s_hcnt_next <= s_hcnt + 1;

  --one clk cycle delay is introduced to capture the correct pixel data
  data_delay_gen : entity ces_util_lib.ces_util_delay
    generic map (
      g_delay  => 1,
      g_data_w => g_data_channels*g_cam_data_w
    )
    port map (
      clk_i  => clk_i,
      ce_i   => '1',
      din_i  => fr_tdata_i,
      dout_o => s_tdata_delay
    );  

  input_gen : for k in 0 to g_data_channels-1 generate
    s_tdata(k) <= s_tdata_delay((k+1)*g_cam_data_w-1 downto k*g_cam_data_w);
  end generate;

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

  --this process is used to infer an encoder that can tell us who out of the 32 data 
  --valid pulses is asserted.
  decoder_proc : process(clk_i)
  begin
    if rising_edge(clk_i) then

      if      s_avg_dv(0) ='1'  then
        s_enc<="00000";
      elsif   s_avg_dv(1) ='1'  then
        s_enc<="00001";
      elsif   s_avg_dv(2) ='1'  then
        s_enc<="00010";
      elsif   s_avg_dv(3) ='1'  then
        s_enc<="00011";
      elsif   s_avg_dv(4) ='1'  then
        s_enc<="00100";
      elsif   s_avg_dv(5) ='1'  then
        s_enc<="00101";
      elsif   s_avg_dv(6) ='1'  then
        s_enc<="00110";
      elsif   s_avg_dv(7) ='1'  then
        s_enc<="00111";
      elsif   s_avg_dv(8) ='1'  then
        s_enc<="01000";
      elsif   s_avg_dv(9) ='1'  then
        s_enc<="01001";
      elsif   s_avg_dv(10)='1'  then
        s_enc<="01010";
      elsif   s_avg_dv(11)='1'  then
        s_enc<="01011";
      elsif   s_avg_dv(12)='1'  then
        s_enc<="01100";
      elsif   s_avg_dv(13)='1'  then
        s_enc<="01101";
      elsif   s_avg_dv(14)='1'  then
        s_enc<="01110";
      elsif   s_avg_dv(15)='1'  then
        s_enc<="01111";
      elsif   s_avg_dv(16)='1'  then
        s_enc<="10000";
      elsif   s_avg_dv(17)='1'  then
        s_enc<="10001";
      elsif   s_avg_dv(18)='1'  then
        s_enc<="10010";
      elsif   s_avg_dv(19)='1'  then
        s_enc<="10011";
      elsif   s_avg_dv(20)='1'  then
        s_enc<="10100";
      elsif   s_avg_dv(21)='1'  then
        s_enc<="10101";
      elsif   s_avg_dv(22)='1'  then
        s_enc<="10110";
      elsif   s_avg_dv(23)='1'  then
        s_enc<="10111";
      elsif   s_avg_dv(24)='1'  then
        s_enc<="11000";
      elsif   s_avg_dv(25)='1'  then
        s_enc<="11001";
      elsif   s_avg_dv(26)='1'  then
        s_enc<="11010";
      elsif   s_avg_dv(27)='1'  then
        s_enc<="11011";
      elsif   s_avg_dv(28)='1'  then
        s_enc<="11100";
      elsif   s_avg_dv(29)='1'  then
        s_enc<="11101";
      elsif   s_avg_dv(30)='1'  then
        s_enc<="11110";
      elsif   s_avg_dv(31)='1'  then
        s_enc<="11111";
      else 
        s_enc<="00000";
      end if;
    end if;
  end process decoder_proc;

  --ram for buffering the averaged results before sending them as a burst to the windowing component
  avg_ram_gen : entity ces_util_lib.ces_util_ram_r_w
    generic map (
      g_ram_latency => 2,
      g_ram_data_w  => g_cam_data_w,
      g_ram_depth   => 32,
      g_init_file   => "",
      g_simulation  => 1
    )
    port map (
      clk_i     => clk_i,
      ena_i     => '1',
      wen_i     => s_b_wen,
      wr_addr_i => s_b_wr_addr,
      wr_dat_i  => s_b_wr_dat,
      enb_i     => '1',
      rd_addr_i => s_b_rd_addr,
      rd_dat_o  => avg_o
    );

  s_b_rd_addr <= std_logic_vector(s_brst_cnt);

--assert s_avg_dv_rise to signify that s_avg_dv has been asserted
  s_avg_dv_re_gen : for m in 0 to 32-1 generate
    process(clk_i)
      begin
        if rising_edge(clk_i) then
          if rst_n_i='0' then
            s_avg_dv_rise(m) <= '0';
          elsif s_vcnt_rst='1' then
            s_avg_dv_rise(m) <= '0';
          elsif s_avg_dv(m)='1' then
            s_avg_dv_rise(m) <= '1';
          end if;
        end if;
      end process;
  end generate;

  --fsm that sends a burst of averaged luminosity data at the output whenever the 31st psa_submodule
  --has send an output data valid (it is assumed that this is when all the average values have been
  --calculated).
  process(clk_i)
  begin
    if rising_edge(clk_i) then
      --mux to write the avg data to the ram
      s_b_wr_dat <= s_avg(to_integer(unsigned(s_enc)));

      if rst_n_i='0' then
        s_brst_reg <= idle;
        s_brst_cnt <= (others => '0');
      else
        case s_brst_reg is 
          when idle =>
            s_out_dv_buff <= '0';
            s_avg_ack <= (others => '0');
            s_brst_cnt <= (others => '0');
            s_b_wen <= '0';
            --start sending data to output as soon as all the average values have been calculated
            if s_avg_dv_or = '1' then
              s_brst_reg <= store_and_ack;
            else
              s_brst_reg <= idle;
            end if;
          when store_and_ack =>
            s_avg_ack(to_integer(unsigned(s_enc))) <= '1';
            s_b_wen <= '1';
            s_b_wr_addr <= s_enc;
            s_brst_reg <= wait_one_clk;
          when wait_one_clk => 
            s_b_wen <= '0';
            if f_vector_and(s_avg_dv_rise)='1' then
              s_brst_reg <= burst;
              s_out_dv_buff <= '1';
            else 
              s_brst_reg <= idle;
            end if;
          when burst =>
            s_avg_ack <= (others => '0');
            if s_brst_cnt=31 then
              s_brst_cnt <= (others => '0');
              s_brst_reg <= wait_for_frame;
              s_out_dv_buff <= '0';
            else
              s_brst_cnt <= s_brst_cnt + 1;
              s_brst_reg <= burst;
              s_out_dv_buff <= '1';
            end if;
          when wait_for_frame =>
            s_avg_ack <= (others => '0'); 
            if fr_vact_i='0' then
              s_brst_reg <= idle;
            else
              s_brst_reg <= wait_for_frame;
            end if;
        end case;
      end if;
    end if;
  end process;

  s_avg_dv_or <= f_vector_or(s_avg_dv);

  --output data valid delay that allows the ram output data to catch up.    
  ces_util_delay_1 : entity ces_util_lib.ces_util_delay
    generic map (
      g_delay  => 2,
      g_data_w => 1
    )
    port map (
      clk_i  => clk_i,
      ce_i   => '1',
      din_i(0)  => s_out_dv_buff,
      dout_o(0) => avg_dv_o
    );  
end rtl_arch;