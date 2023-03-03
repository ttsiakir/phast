--==============================================================================
-- Module Name : ces_video_hdmi_timing
-- Library     : ces_image_lib
-- Project     : CES HDMI CONTROLLER
-- Company     : Campera Electronic Systems Srl
-- Author      : GDM
--------------------------------------------------------------------------------
-- Description  : This module is the HDMI timing controller
--------------------------------------------------------------------------------
-- (c) Copyright 2020 Campera Electronic Systems Srl. Via M. Giuntini, 63
-- Navacchio (Pisa), 56023, Italy. <www.campera-es.com>. All rights reserved. 
-- THIS COPYRIGHT NOTICE MUST BE RETAINED AS PART OF THIS FILE AT ALL TIMES.
--------------------------------------------------------------------------------
-- Revision History:
-- Date         Version    Author         Description
-- 04/03/2018   1.0.0      GDM            Initial release   
--==============================================================================

library ieee;  
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library ces_util_lib;
use ces_util_lib.ces_util_pkg.all; 

entity ces_video_hdmi_timing is
  generic (
    g_v_front  : natural   := 4;
    g_v_synch  : natural   := 5;
    g_v_back   : natural   := 36;
    g_v_active : natural   := 1080;
    g_v_pol    : std_logic := '1';
    g_h_front  : natural   := 88;
    g_h_sync   : natural   := 44;
    g_h_back   : natural   := 148;
    g_h_active : natural   := 1920;
    g_h_pol    : std_logic := '1';
    g_data_w   : integer := 8;
    g_data_ch   : integer := 4
    );
  port (
    hdmi_clk_i   : in std_logic;
    rst_i        : in std_logic;
    
    hdmi_vact_o  : out std_logic;
    hdmi_hact_o  : out std_logic;
    hdmi_de_o    : out std_logic;
    hdmi_vsync_o : out std_logic;
    hdmi_hsync_o : out std_logic;
    
    hdmi_tdata : out std_logic_vector(g_data_ch*g_data_w - 1 downto 0)
    
    );
end entity ces_video_hdmi_timing;

architecture a_rtl of ces_video_hdmi_timing is
  constant C_V_BLANK : natural := g_v_front + g_v_synch + g_v_back;
  constant C_H_BLANK : natural := g_h_front + g_h_sync + g_h_back;
  
  signal s_v_isactive : std_logic;
  signal s_h_isactive : std_logic;

  signal s_hcnt: unsigned(12 downto 0);
  signal s_vcnt: unsigned(12 downto 0);

  signal s_colour_data : std_logic_vector(g_data_ch*g_data_w - 1 downto 0);
begin
  
  proc_timing_gen: process(hdmi_clk_i) is
  begin
    if hdmi_clk_i'event and hdmi_clk_i = '1' then
      if rst_i = '0' then
        hdmi_vsync_o <= not g_v_pol;
        hdmi_hsync_o <= not g_h_pol;
        s_h_isactive   <= '0';
        s_v_isactive   <= '0';
        s_hcnt       <= to_unsigned(1, 13);
        s_vcnt       <= to_unsigned(1, 13);
      else
      --send the appropriate colour value to the pixels 
        hdmi_tdata <= s_colour_data;
        if s_hcnt = g_h_front then
          hdmi_hsync_o <= g_h_pol;
        end if;
        
        if s_hcnt = g_h_front + g_h_sync then
          hdmi_hsync_o <= not g_h_pol;
        end if;
        
        if s_hcnt = c_h_blank then
          s_h_isactive <= '1';
        end if;
        
        if s_hcnt = c_h_blank + g_h_active then
          s_h_isactive <= '0';
          s_hcnt <= to_unsigned(1, 13);
          
          if s_vcnt = g_v_front then
            hdmi_vsync_o <= g_v_pol;
          end if;
          
          if s_vcnt = g_v_front + g_v_synch then
            hdmi_vsync_o <= not g_v_pol;
          end if;
          
          if s_vcnt = C_V_BLANK then
            s_v_isactive <= '1';
          end if;
          
          if s_vcnt = C_V_BLANK + g_v_active then
            s_v_isactive <= '0';
            s_vcnt <= to_unsigned(1, 13);
          else
            s_vcnt <= s_vcnt + 1;
          end if;
          
        else
          s_hcnt <= s_hcnt + 1;
        end if;
        
      end if;
    end if;
  end process proc_timing_gen;  

  random_data_gen : entity ces_util_lib.ces_util_lfsr
    generic map (
      g_data_w => g_data_ch*g_data_w
    )
    port map (
      clk_i   => hdmi_clk_i,
      rst_n_i => rst_i,
      load_i  => '0',
      seed_i  => (others => '1'),
      rng_o   => s_colour_data
    );
  
  hdmi_de_o <= (s_h_isactive and s_v_isactive);
  
  hdmi_hact_o <= s_h_isactive;
  hdmi_vact_o <= s_v_isactive;  
  
end a_rtl;