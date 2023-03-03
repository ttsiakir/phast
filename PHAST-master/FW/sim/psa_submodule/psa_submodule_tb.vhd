--------------------------------------------------------------------------------
-- Title       : <Title Block>
-- Project     : Default Project Name
--------------------------------------------------------------------------------
-- File        : psa_submodule_tb.vhd
-- Author      : User Name <user.email@user.company.com>
-- Company     : User Company Name
-- Created     : Wed Nov  3 14:24:11 2021
-- Last update : Fri Nov  5 14:26:31 2021
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

library ces_util_lib;
use ces_util_lib.ces_util_pkg.all; 

-----------------------------------------------------------

entity psa_submodule_tb is

end entity psa_submodule_tb;

-----------------------------------------------------------

architecture testbench of psa_submodule_tb is

	-- Testbench DUT generics
	constant g_cam_data_w         : integer := 13;
	constant g_max_nop_per_subreg : integer := 8;

	-- Testbench DUT ports
	signal clk_i : std_logic:='0';
	signal rst_i : std_logic;
	signal pnt_lum_i : std_logic_vector(g_max_nop_per_subreg*g_cam_data_w-1 downto 0) := x"0AB2345FF3157561ABC578921F";
	signal pnt_nop_i : std_logic_vector(f_ceil_log2(g_max_nop_per_subreg)-1 downto 0) := "111";
	signal pnt_dv_i  : std_logic;
	signal avg_o     : std_logic_vector(g_cam_data_w+2 downto 0);
	signal avg_dv_o     : std_logic:='0';

	--signal tmp: unsigned(g_cam_data_w+2 downto 0);

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
		rst_i <= '0',
		         '1' after 20.0*C_CLK_PERIOD * (1 SEC);
		wait;
	end process RESET_GEN;

	-----------------------------------------------------------
	-- Testbench Stimulus
	-----------------------------------------------------------
	STIMULUS : process
	begin
		pnt_nop_i <= "000",
		         "100" after 20.0*C_CLK_PERIOD * (1 SEC),
		         "111" after 50.0*C_CLK_PERIOD * (1 SEC);

		pnt_lum_i <= x"0AB23FFFF3157561ABC578921F" after 100.0*C_CLK_PERIOD * (1 SEC);
		pnt_dv_i <= '0', '1' after 100.0*C_CLK_PERIOD * (1 SEC),'0' after 101.0*C_CLK_PERIOD * (1 SEC);
		wait;
	end process STIMULUS;
	-----------------------------------------------------------
	-- Entity Under Test
	-----------------------------------------------------------
	DUT : entity work.psa_submodule
		generic map (
			g_cam_data_w         => g_cam_data_w,
			g_max_nop_per_subreg => g_max_nop_per_subreg
		)
		port map (
			clk_i => clk_i,
			rst_i => rst_i,
			pnt_lum_i => pnt_lum_i,
			pnt_nop_i => pnt_nop_i,
			pnt_dv_i  => pnt_dv_i,
			avg_o     => avg_o,
			avg_dv_o  => avg_dv_o
		);

		--tmp <= (unsigned("000" & pnt_lum_i(8*g_cam_data_w-1 downto 7*g_cam_data_w)) +
        --          unsigned("000" & pnt_lum_i(7*g_cam_data_w-1 downto 6*g_cam_data_w)))+
        --         (unsigned("000" & pnt_lum_i(6*g_cam_data_w-1 downto 5*g_cam_data_w)) +
        --          unsigned("000" & pnt_lum_i(5*g_cam_data_w-1 downto 4*g_cam_data_w)))+
        --         (unsigned("000" & pnt_lum_i(4*g_cam_data_w-1 downto 3*g_cam_data_w)) +
        --          unsigned("000" & pnt_lum_i(3*g_cam_data_w-1 downto 2*g_cam_data_w)))+
        --         (unsigned("000" & pnt_lum_i(2*g_cam_data_w-1 downto 1*g_cam_data_w)) +
        --          unsigned("000" & pnt_lum_i(1*g_cam_data_w-1 downto 0)));

end architecture testbench;