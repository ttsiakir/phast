--------------------------------------------------------------------------------
-- Title       : <Title Block>
-- Project     : Default Project Name
--------------------------------------------------------------------------------
-- File        : ces_video_hdmi_timing_tb.vhd
-- Author      : User Name <user.email@user.company.com>
-- Company     : User Company Name
-- Created     : Fri Dec 17 18:38:49 2021
-- Last update : Fri Jan 21 17:00:53 2022
-- Platform    : Default Part Number
-- Standard    : <VHDL-2008 | VHDL-2002 | VHDL-1993 | VHDL-1987>
--------------------------------------------------------------------------------
-- Copyright (c) 2021 User Company Name
-------------------------------------------------------------------------------
-- Description: 
--------------------------------------------------------------------------------
-- Revisions:  Revisions and documentation are controlled by
-- the revision control system (RCS).  The RCS should be consulted
-- on revision history.
-------------------------------------------------------------------------------
--C:\project\gitlab\PHAST\PHAST\FW\CoaXPress\coaxpress_host_nobuild_-_2021_10_14\template\vhdl\gt
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use ieee.std_logic_textio.all;
use ieee.math_real.all;
library ces_util_lib;
use ces_util_lib.ces_util_pkg.all;

-----------------------------------------------------------

entity ces_video_hdmi_timing_tb is

end entity ces_video_hdmi_timing_tb;

-----------------------------------------------------------

architecture testbench of ces_video_hdmi_timing_tb is

	-- Testbench DUT generics
	constant g_cam_data_w   : integer:= 8;
	constant g_data_ch  : integer   := 4;
    --number of points per subregion
    constant g_max_nop_per_subreg : integer := 8;
    --data channles that come from the AXI bus (power of two)
    constant g_data_channels : integer := 4;
    --maximum camera frame dimensions (default settings : 8k)
    constant g_v_active_max : integer := 4320;
    constant g_h_active_max : integer := 7680;
    constant g_h_active : integer := 7680/g_data_ch;

	-- Testbench DUT ports
	signal clk_i        : std_logic;
	signal rst_i        : std_logic;
	signal hdmi_vact_o  : std_logic;
	signal hdmi_hact_o  : std_logic;
	signal hdmi_de_o    : std_logic;
	signal hdmi_vsync_o : std_logic;
	signal hdmi_hsync_o : std_logic;
	signal hdmi_tdata   : std_logic_vector(g_data_ch*g_cam_data_w - 1 downto 0);

	-- Other constants
	constant C_CLK_PERIOD : real := 1.0e-9; -- NS

	signal pnt_h_i    : std_logic_vector(g_max_nop_per_subreg*f_ceil_log2(g_h_active_max)-1 downto 0);
	signal pnt_v_i    : std_logic_vector(g_max_nop_per_subreg*f_ceil_log2(g_v_active_max)-1 downto 0);
	signal pnt_nos_i  : std_logic_vector(5-1 downto 0);
	signal pnt_nop_i  : std_logic_vector(32*f_ceil_log2(g_max_nop_per_subreg)-1 downto 0) := (others => '1');
	signal pnt_dv_i   : std_logic;
	signal avg_o      : std_logic_vector(g_cam_data_w-1 downto 0);--
	signal avg_dv     : std_logic;	

	signal temp : unsigned(12 downto 0) := (others => '0');--unsigned(f_ceil_log2(g_v_active_max)-1 downto 0);
	signal temp2 : unsigned(12 downto 0) := (others => '0');

begin
	-----------------------------------------------------------
	-- Clocks and Reset
	-----------------------------------------------------------
	CLK_GEN : process
	begin
		clk_i <= '1';
		wait for C_CLK_PERIOD / 2.0 * (1 SEC);
		clk_i <= '0';
		wait for C_CLK_PERIOD / 2.0 * (1 SEC);
	end process CLK_GEN;

	RESET_GEN : process
	begin
		rst_i <= '0',
		         '1' after 20.0*C_CLK_PERIOD * (1 SEC);
		wait;
	end process RESET_GEN;

	-----------------------------------------------------------
	-- Testbench Stimulus
	-----------------------------------------------------------

	-----------------------------------------------------------
	-- Entity Under Test
	-----------------------------------------------------------
	frame_emmulator : entity work.ces_video_hdmi_timing
		generic map (
			g_v_front  => 4,
			g_v_synch  => 5,
			g_v_back   => 36,
			g_v_active => 4320,
			g_v_pol    => '1',
			g_h_front  => 10,
			g_h_sync   => 5,
			g_h_back   => 5,
			g_h_active => g_h_active,
			g_h_pol    => '1',
			g_data_w   => g_cam_data_w,
			g_data_ch  => g_data_ch
		)
		port map (
			hdmi_clk_i   => clk_i,
			rst_i        => rst_i,
			hdmi_vact_o  => hdmi_vact_o,
			hdmi_hact_o  => hdmi_hact_o,
			hdmi_de_o    => hdmi_de_o,
			hdmi_vsync_o => hdmi_vsync_o,
			hdmi_hsync_o => hdmi_hsync_o,
			hdmi_tdata   => hdmi_tdata
		);

	psa : entity work.psa
		generic map (
			g_cam_data_w         => g_cam_data_w,
			g_max_nop_per_subreg => g_max_nop_per_subreg,
			g_data_channels      => g_data_channels,
			g_v_active_max       => g_v_active_max,
			g_h_active_max       => g_h_active_max
		)
		port map (
			clk_i      => clk_i,
			rst_n_i    => rst_i,
			pnt_h_i    => pnt_h_i,
			pnt_v_i    => pnt_v_i,
			pnt_nos_i  => pnt_nos_i,
			pnt_nop_i  => pnt_nop_i,
			pnt_dv_i   => pnt_dv_i,
			fr_tdata_i => hdmi_tdata,
			fr_vact_i  => hdmi_vact_o,
			fr_hact_i  => hdmi_hact_o,
			fr_de_i    => hdmi_de_o,
			fr_vsync_i => hdmi_vsync_o,
			fr_hsync_i => hdmi_hsync_o,
			avg_o      => avg_o,
			avg_dv_o   => avg_dv
		);	
-------------------|------------------------------------------|------------------
-------------------|PROCESS THAT SELECTS PIXELS FROM THE FRAME|------------------
-------------------V------------------------------------------V------------------
	pix_selec_proc : process
	begin
		pnt_nos_i <= (others => '0');
		pnt_h_i<=("00000011100110000000001010000000100001000000001010100000001100000000000010000100000000001100000001110101");
		pnt_v_i<=("00000001000100000000101011000000001011100000000101100000000110101000000010010100000000001100000000111100");
		wait until clk_i='1';
		pnt_dv_i <= '1';
		wait until clk_i='1';
		pnt_dv_i <= '0';
		pnt_nos_i <= std_logic_vector(unsigned(pnt_nos_i) + 1);
		pnt_h_i<=("00000010111000000001101010000000010111100000000011100000001110111000000001101100000000111100000000111001");
		pnt_v_i<=("00000001101000000000010011000000010110000000000010010000000001111000000010111000000000100010000000101011");
		wait until clk_i='1';
		pnt_dv_i <= '1';
		wait until clk_i='1';
		pnt_dv_i <= '0';
		pnt_nos_i <= std_logic_vector(unsigned(pnt_nos_i) + 1);
		pnt_h_i<=("00000001101000000000010000000000000101000000011100000000001010010000000101001100000001010100000001000010");
		pnt_v_i<=("00000001111010000001000000000000000100000000001100000000000111000000000011101100000000010010000000110001");
		wait until clk_i='1';
		pnt_dv_i <= '1';
		wait until clk_i='1';
		pnt_dv_i <= '0';
		pnt_nos_i <= std_logic_vector(unsigned(pnt_nos_i) + 1);
		pnt_h_i<=("00000011011000000000111111000000011001000000000001110000001110010000000010100000000000010000000000111011");
		pnt_v_i<=("00000000101010000000101111000000000111100000000100000000000001001000000011011000000000111100000000101010");
		wait until clk_i='1';
		pnt_dv_i <= '1';
		wait until clk_i='1';
		pnt_dv_i <= '0';
		pnt_nos_i <= std_logic_vector(unsigned(pnt_nos_i) + 1);
		pnt_h_i<=("00000011001000000000101001000000111000100000000101000000010000000000000010100000000010111110000000111110");
		pnt_v_i<=("00000001000100000000011100000000001101100000001100100000000010011000000001110000000001101010000000101111");
		wait until clk_i='1';
		pnt_dv_i <= '1';
		wait until clk_i='1';
		pnt_dv_i <= '0';
		pnt_nos_i <= std_logic_vector(unsigned(pnt_nos_i) + 1);
		pnt_h_i<=("00000000101000000001010101000000001110000000010011110000000111101000000111011100000010000100000000111000");
		pnt_v_i<=("00000001101100000000101010000000011111100000000110110000000101010000000000110000000001100100000000010100");
		wait until clk_i='1';
		pnt_dv_i <= '1';
		wait until clk_i='1';
		pnt_dv_i <= '0';
		pnt_nos_i <= std_logic_vector(unsigned(pnt_nos_i) + 1);
		pnt_h_i<=("00000011001000000001011110000000111111000000011011000000001111001000000110111000000000011000000001111000");
		pnt_v_i<=("00000001111100000000111010000000011011100000001110000000000110001000000010100100000000101110000000111100");
		wait until clk_i='1';
		pnt_dv_i <= '1';
		wait until clk_i='1';
		pnt_dv_i <= '0';
		pnt_nos_i <= std_logic_vector(unsigned(pnt_nos_i) + 1);
		pnt_h_i<=("00000011001000000000101011000000010100000000011011010000000101010000000110001000000000101100000000101010");
		pnt_v_i<=("00000001000110000000011111000000010110100000001100100000000100010000000011101100000001000010000000100101");
		wait until clk_i='1';
		pnt_dv_i <= '1';
		wait until clk_i='1';
		pnt_dv_i <= '0';
		pnt_nos_i <= std_logic_vector(unsigned(pnt_nos_i) + 1);
		pnt_h_i<=("00000010111110000001010010000000100000100000011010000000001100101000000110111100000001101010000000011101");
		pnt_v_i<=("00000001101010000000101110000000000111000000001000010000000010010000000001000000000001001010000000100110");
		wait until clk_i='1';
		pnt_dv_i <= '1';
		wait until clk_i='1';
		pnt_dv_i <= '0';
		pnt_nos_i <= std_logic_vector(unsigned(pnt_nos_i) + 1);
		pnt_h_i<=("00000001100000000001001001000000011010100000000011110000000010001000000111001100000000011010000000111100");
		pnt_v_i<=("00000001010010000000111101000000000010000000001000010000000100000000000010010000000000011010000000110101");
		wait until clk_i='1';
		pnt_dv_i <= '1';
		wait until clk_i='1';
		pnt_dv_i <= '0';
		pnt_nos_i <= std_logic_vector(unsigned(pnt_nos_i) + 1);
		pnt_h_i<=("00000010000100000001001100000000001101100000001011010000000001001000000101010100000000101000000001111111");
		pnt_v_i<=("00000001100010000000101010000000011000100000001100100000000111001000000001010000000000001010000000001111");
		wait until clk_i='1';
		pnt_dv_i <= '1';
		wait until clk_i='1';
		pnt_dv_i <= '0';
		pnt_nos_i <= std_logic_vector(unsigned(pnt_nos_i) + 1);
		pnt_h_i<=("00000011001110000000011101000000100100100000001011010000001101011000000111001000000011111110000000110110");
		pnt_v_i<=("00000001101110000000000100000000011011000000001111100000000001011000000010111000000000010110000000110101");
		wait until clk_i='1';
		pnt_dv_i <= '1';
		wait until clk_i='1';
		pnt_dv_i <= '0';
		pnt_nos_i <= std_logic_vector(unsigned(pnt_nos_i) + 1);
		pnt_h_i<=("00000001101010000000101110000000010101000000011010010000000100011000000000100100000001110010000000010001");
		pnt_v_i<=("00000000010000000000110001000000001111100000000101110000000100100000000000101000000001010110000000011001");
		wait until clk_i='1';
		pnt_dv_i <= '1';
		wait until clk_i='1';
		pnt_dv_i <= '0';
		pnt_nos_i <= std_logic_vector(unsigned(pnt_nos_i) + 1);
		pnt_h_i<=("00000011001000000000000100000000010001100000011010110000001011011000001000000000000000100100000000011010");
		pnt_v_i<=("00000001100110000000000110000000011000000000001111110000000100111000000011000100000001111110000000011111");
		wait until clk_i='1';
		pnt_dv_i <= '1';
		wait until clk_i='1';
		pnt_dv_i <= '0';
		pnt_nos_i <= std_logic_vector(unsigned(pnt_nos_i) + 1);
		pnt_h_i<=("00000011000110000000111110000000101010100000001110100000001000100000000111100000000010010000000001001101");
		pnt_v_i<=("00000001011100000000101000000000000010100000001010010000000010011000000010000100000000101010000000100100");
		wait until clk_i='1';
		pnt_dv_i <= '1';
		wait until clk_i='1';
		pnt_dv_i <= '0';
		pnt_nos_i <= std_logic_vector(unsigned(pnt_nos_i) + 1);
		pnt_h_i<=("00000010001000000001101110000000011010100000001011010000001001100000000011001000000000111010000000100010");
		pnt_v_i<=("00000000010000000000101001000000011110100000001011010000000001011000000000110000000001001110000000100010");
		wait until clk_i='1';
		pnt_dv_i <= '1';
		wait until clk_i='1';
		pnt_dv_i <= '0';
		pnt_nos_i <= std_logic_vector(unsigned(pnt_nos_i) + 1);
		pnt_h_i<=("00000000010110000000101011000000101011000000000110110000000100001000000000010100000000110010000000011110");
		pnt_v_i<=("00000000111100000000110001000000001011100000001011010000000101001000000011100100000000100000000000001101");
		wait until clk_i='1';
		pnt_dv_i <= '1';
		wait until clk_i='1';
		pnt_dv_i <= '0';
		pnt_nos_i <= std_logic_vector(unsigned(pnt_nos_i) + 1);
		pnt_h_i<=("00000011111100000001011111000000011110000000010101100000001001001000000011010000000001101000000000110100");
		pnt_v_i<=("00000000011000000000100100000000001010100000001110010000000010011000000001001000000001000000000001000000");
		wait until clk_i='1';
		pnt_dv_i <= '1';
		wait until clk_i='1';
		pnt_dv_i <= '0';
		pnt_nos_i <= std_logic_vector(unsigned(pnt_nos_i) + 1);
		pnt_h_i<=("00000011001010000001001001000000001111100000000100010000000011101000000110101100000000100000000001000000");
		pnt_v_i<=("00000001011110000000101111000000011110000000000010100000000101110000000000100000000000101000000000111001");
		wait until clk_i='1';
		pnt_dv_i <= '1';
		wait until clk_i='1';
		pnt_dv_i <= '0';
		pnt_nos_i <= std_logic_vector(unsigned(pnt_nos_i) + 1);
		pnt_h_i<=("00000010011010000000101110000000101100100000000110010000001110101000000111110000000010000110000000111000");
		pnt_v_i<=("00000000010100000000110101000000001010000000001000010000000111001000000001010000000000100010000000000110");
		wait until clk_i='1';
		pnt_dv_i <= '1';
		wait until clk_i='1';
		pnt_dv_i <= '0';
		pnt_nos_i <= std_logic_vector(unsigned(pnt_nos_i) + 1);
		pnt_h_i<=("00000001101110000000100001000000101001100000010000110000001000100000000000110000000001010100000001010100");
		pnt_v_i<=("00000000100110000000101100000000010010000000000101000000000010011000000010111000000001000000000000110101");
		wait until clk_i='1';
		pnt_dv_i <= '1';
		wait until clk_i='1';
		pnt_dv_i <= '0';
		pnt_nos_i <= std_logic_vector(unsigned(pnt_nos_i) + 1);
		pnt_h_i<=("00000011111110000001011101000000001100100000001111010000000111111000000101101100000001110000000001110110");
		pnt_v_i<=("00000001010100000000001100000000010001100000001001010000000100111000000010101100000000001010000000001110");
		wait until clk_i='1';
		pnt_dv_i <= '1';
		wait until clk_i='1';
		pnt_dv_i <= '0';
		pnt_nos_i <= std_logic_vector(unsigned(pnt_nos_i) + 1);
		pnt_h_i<=("00000001101010000001000000000000100010100000011001010000001001111000000101101000000000011110000001000011");
		pnt_v_i<=("00000001010100000001000000000000011001100000001001110000000110100000000001111000000001011110000000111111");
		wait until clk_i='1';
		pnt_dv_i <= '1';
		wait until clk_i='1';
		pnt_dv_i <= '0';
		pnt_nos_i <= std_logic_vector(unsigned(pnt_nos_i) + 1);
		pnt_h_i<=("00000001010100000001000011000000100101000000010001000000000110111000000101101000000001110010000000011000");
		pnt_v_i<=("00000000111100000000100110000000011100000000001000010000000111011000000000100000000001111100000000100100");
		wait until clk_i='1';
		pnt_dv_i <= '1';
		wait until clk_i='1';
		pnt_dv_i <= '0';
		pnt_nos_i <= std_logic_vector(unsigned(pnt_nos_i) + 1);
		pnt_h_i<=("00000000110110000001100110000000011011000000000111000000001100011000000000010000000000100000000000000101");
		pnt_v_i<=("00000001101010000000100101000000000011100000000111100000000100000000000000010000000000001000000000010011");
		wait until clk_i='1';
		pnt_dv_i <= '1';
		wait until clk_i='1';
		pnt_dv_i <= '0';
		pnt_nos_i <= std_logic_vector(unsigned(pnt_nos_i) + 1);
		pnt_h_i<=("00000000101000000000101111000000010110100000011000100000000001001000000100001100000001011100000000010110");
		pnt_v_i<=("00000000010000000000110010000000000011000000001101010000000001011000000000101100000001010100000000000111");
		wait until clk_i='1';
		pnt_dv_i <= '1';
		wait until clk_i='1';
		pnt_dv_i <= '0';
		pnt_nos_i <= std_logic_vector(unsigned(pnt_nos_i) + 1);
		pnt_h_i<=("00000011110100000000011100000000100111100000001010100000000010001000000110101100000011100110000000111100");
		pnt_v_i<=("00000001011010000000111000000000000100000000001100010000000001000000000011011000000001101010000000001011");
		wait until clk_i='1';
		pnt_dv_i <= '1';
		wait until clk_i='1';
		pnt_dv_i <= '0';
		pnt_nos_i <= std_logic_vector(unsigned(pnt_nos_i) + 1);
		pnt_h_i<=("00000001010110000001010010000000001001000000010010010000001011100000000100101100000000110000000001101011");
		pnt_v_i<=("00000000110110000000111011000000001011000000001100000000000010111000000011000000000001110110000000101001");
		wait until clk_i='1';
		pnt_dv_i <= '1';
		wait until clk_i='1';
		pnt_dv_i <= '0';
		pnt_nos_i <= std_logic_vector(unsigned(pnt_nos_i) + 1);
		pnt_h_i<=("00000011001000000000001000000000001010000000000010110000000111101000000000110100000001011000000001100100");
		pnt_v_i<=("00000001001000000000111001000000001001100000000100000000000001011000000000111100000001111000000001000000");
		wait until clk_i='1';
		pnt_dv_i <= '1';
		wait until clk_i='1';
		pnt_dv_i <= '0';
		pnt_nos_i <= std_logic_vector(unsigned(pnt_nos_i) + 1);
		pnt_h_i<=("00000001100010000001010110000000011010000000000011110000000010011000000110000000000000110100000000000101");
		pnt_v_i<=("00000000001000000000100100000000010000000000001000100000000011101000000001000000000000100110000000011100");
		wait until clk_i='1';
		pnt_dv_i <= '1';
		wait until clk_i='1';
		pnt_dv_i <= '0';
		pnt_nos_i <= std_logic_vector(unsigned(pnt_nos_i) + 1);
		pnt_h_i<=("00000000100000000001100000000000110100000000001001010000001101101000000011010100000011011110000000010001");
		pnt_v_i<=("00000001100010000000010101000000011111000000001101000000000001001000000011011100000000001000000001000000");
		wait until clk_i='1';
		pnt_dv_i <= '1';
		wait until clk_i='1';
		pnt_dv_i <= '0';
		pnt_nos_i <= std_logic_vector(unsigned(pnt_nos_i) + 1);
		pnt_h_i<=("00000000100000000001100000000000110100000000001001010000001101101000000011010100000011011110000000010001");
		pnt_v_i<=("00000001100010000000010101000000011111000000001101000000000001001000000011011100000000001000000001000000");
		wait until clk_i='1';
		pnt_dv_i <= '1';
		wait until clk_i='1';
		pnt_dv_i <= '0';
		pnt_nos_i <= std_logic_vector(unsigned(pnt_nos_i) + 1);
		wait;
	end process pix_selec_proc;

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

end architecture testbench;