--------------------------------------------------------------------------------
-- Title       : <Title Block>
-- Project     : Default Project Name
--------------------------------------------------------------------------------
-- File        : psa_tb.vhd
-- Author      : User Name <user.email@user.company.com>
-- Company     : User Company Name
-- Created     : Tue Nov  2 14:15:23 2021
-- Last update : Mon Nov 15 15:37:33 2021
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

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use ieee.std_logic_textio.all;
use ieee.math_real.all;


library ces_util_lib;
use ces_util_lib.ces_util_pkg.all; 
-----------------------------------------------------------

entity psa_tb is

end entity psa_tb;

-----------------------------------------------------------

architecture testbench of psa_tb is

	-- Testbench DUT generics
	constant g_cam_data_w : integer := 8;
    constant g_max_nop_per_subreg : integer := 8;

	constant g_v_front  : natural   := 4;
	constant g_v_synch  : natural   := 5;
	constant g_v_back   : natural   := 36;
	constant g_v_active : natural   := 128;
	constant g_v_pol    : std_logic := '1';
	constant g_h_front  : natural   := 10;
	constant g_h_sync   : natural   := 5;
	constant g_h_back   : natural   := 5;
	constant g_h_active : natural   := 64;
	constant g_h_pol    : std_logic := '1';

	-- Testbench DUT ports
	signal clk_i        : std_logic;
	signal rst_i        : std_logic;
	--signal cam_subreg_i : std_logic_vector(4 downto 0);
	signal pnt_h_i      : std_logic_vector(g_max_nop_per_subreg*f_ceil_log2(128)-1 downto 0)  := x"5f90c2baf848fF";
	signal pnt_v_i      : std_logic_vector(g_max_nop_per_subreg*f_ceil_log2(64)-1 downto 0) :=  x"B2D34BC2DC0E";
	signal pnt_nos_i    : std_logic_vector(5-1 downto 0):="00111";
	signal pnt_nop_i    : std_logic_vector(32*f_ceil_log2(g_max_nop_per_subreg)-1 downto 0):=x"ABCF9873AAF8929492919AAB";
	signal pnt_dv_i     : std_logic;

	--camera immulator
	signal hdmi_vact_i  : std_logic;
	signal hdmi_hact_i  : std_logic;
	signal hdmi_de_i    : std_logic;
	signal hdmi_vsync_o : std_logic;
	signal hdmi_hsync_o : std_logic;
	signal hdmi_tdata   : std_logic_vector(g_cam_data_w-1 downto 0);
	signal avg_o        : std_logic_vector(255 downto 0);
	signal avg_dv_o     : std_logic_vector(31 downto 0);

	-- Other constants
	constant C_CLK_PERIOD : real := 10.0e-9; -- NS

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
		rst_i <= '1',
		         '0' after 20.0* C_CLK_PERIOD * (1 SEC),
		         '1' after 25.0 * C_CLK_PERIOD * (1 SEC);
		wait;
	end process RESET_GEN;

	-----------------------------------------------------------
	-- Testbench Stimulus
	-----------------------------------------------------------
	process
	variable seed1, seed2 : positive;
	variable rand : real;
	variable int_rand: integer;
	variable temp : std_logic_vector(g_max_nop_per_subreg*f_ceil_log2(128)-1 downto 0);
	begin
		pnt_dv_i<= '0';
		pnt_nos_i <= (others => '0');
		wait until rising_edge(rst_i);
		wait until clk_i='1';
		wait for C_CLK_PERIOD*(1 SEC);

		for k in 0 to 32-1 loop
			for j in 0 to 7 loop
				uniform(seed1,seed2,rand);
				int_rand := integer(trunc(rand*4096.0));
				temp((j+1)*f_ceil_log2(128)-1 downto j*f_ceil_log2(128)) := std_logic_vector(to_unsigned(int_rand, f_ceil_log2(128)));
			end loop;
			pnt_h_i <= temp;
			for j in 0 to 7 loop
				uniform(seed1,seed2,rand);
				int_rand := integer(trunc(rand*4096.0));
				temp((j+1)*f_ceil_log2(128)-1 downto j*f_ceil_log2(128)) := std_logic_vector(to_unsigned(int_rand, f_ceil_log2(128)));
			end loop;
			pnt_v_i <= temp(g_max_nop_per_subreg*f_ceil_log2(64)-1 downto 0);
			pnt_dv_i <= '1';
			wait for C_CLK_PERIOD*(1 SEC);
			pnt_dv_i <= '0';
			pnt_nos_i <= std_logic_vector(unsigned(pnt_nos_i) + 1);
			wait for C_CLK_PERIOD*(1 SEC);
		end loop;
		wait;
	end process;
	-----------------------------------------------------------
	-- Entity Under Test
	-----------------------------------------------------------
	DUT : entity work.psa
		generic map (
			g_cam_data_w => g_cam_data_w,
            g_max_nop_per_subreg => g_max_nop_per_subreg
		)
		port map (
			clk_i        => clk_i,
			rst_i        => rst_i,
			pnt_h_i      => pnt_h_i,
			pnt_v_i      => pnt_v_i,
			pnt_nos_i    => pnt_nos_i,
			pnt_nop_i    => pnt_nop_i,
			pnt_dv_i     => pnt_dv_i,
			fr_vact_i    => hdmi_vact_i,
			fr_hact_i    => hdmi_hact_i,
			fr_de_i      => hdmi_de_i,
			fr_vsync_i   => hdmi_vsync_o,
			fr_hsync_i   => hdmi_hsync_o,
			fr_tdata_i   => hdmi_tdata,
			avg_o        => avg_o,
			avg_dv_o     => avg_dv_o
		);

	ces_video_hdmi_timing_1 : entity work.ces_video_hdmi_timing
		generic map (
			g_v_front    => g_v_front,
			g_v_synch    => g_v_synch,
			g_v_back     => g_v_back,
			g_v_active   => g_v_active,
			g_v_pol      => g_v_pol,
			g_h_front    => g_h_front,
			g_h_sync     => g_h_sync,
			g_h_back     => g_h_back,
			g_h_active   => g_h_active,
			g_h_pol      => g_h_pol,
			g_data_w     => g_cam_data_w
		)
		port map (
			hdmi_clk_i   => clk_i,
			rst_i        => rst_i,
			hdmi_vact_o  => hdmi_vact_i,
			hdmi_hact_o  => hdmi_hact_i,
			hdmi_de_o    => hdmi_de_i,
			hdmi_vsync_o => hdmi_vsync_o,
			hdmi_hsync_o => hdmi_hsync_o,
			hdmi_tdata   => hdmi_tdata
		);	

end architecture testbench;