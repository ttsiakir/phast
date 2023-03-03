--------------------------------------------------------------------------------
-- Title       : <Title Block>
-- Project     : Default Project Name
--------------------------------------------------------------------------------
-- File        : psa_submodule_tb.vhd
-- Author      : User Name <user.email@user.company.com>
-- Company     : User Company Name
-- Created     : Wed Nov  3 14:24:11 2021
-- Last update : Wed Jan 19 18:47:59 2022
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
	constant g_cam_data_w         : integer := 8;
	constant g_max_nop_per_subreg : integer := 8;

	-- Testbench DUT ports
	signal clk_i : std_logic:='0';
	signal rst_i : std_logic;
	signal pnt_lum_i : std_logic_vector(g_max_nop_per_subreg*g_cam_data_w-1 downto 0) := (others=>'1');--:= x"0AB2345FF3157561ABC578921F";
	signal pnt_nop_i : std_logic_vector(f_ceil_log2(g_max_nop_per_subreg)-1 downto 0) := "111";
	signal pnt_dv_i  : std_logic := '0';
	signal avg_o     : std_logic_vector(g_cam_data_w-1 downto 0);
	signal avg_dv_o  : std_logic:='0';
	signal ack : std_logic := '0';

	--signal tmp: unsigned(g_cam_data_w+2 downto 0);

	-- Other constants
	constant C_CLK_PERIOD : real := 10.0e-9; -- NS

	signal counter : integer := 0;

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
		--pnt_nop_i <= "000",
		--         "100" after 20.0*C_CLK_PERIOD * (1 SEC),
		--         "111" after 50.0*C_CLK_PERIOD * (1 SEC);

		wait until counter = 44;
		wait until clk_i='1';
		pnt_lum_i <=x"8fdd9bfc0f38f421";
		pnt_dv_i <= '1';
		wait for C_CLK_PERIOD * (1 SEC);
		wait until clk_i='1';
		pnt_dv_i <= '0';
		wait until avg_dv_o='1';
		wait until clk_i='1';
		ack <= '1';
		wait until clk_i='1';
		ack <= '0';
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
			clk_i     => clk_i,
			rst_n_i   => rst_i,
			pnt_lum_i => pnt_lum_i,
			pnt_nop_i => pnt_nop_i,
			pnt_dv_i  => pnt_dv_i,
			avg_o     => avg_o,
			avg_dv_o  => avg_dv_o,
			avg_ack_i => ack
		);

	tb_counter_proc : process(clk_i)
	begin
		if rising_edge(clk_i) then 
			counter <= counter + 1;
		end if;
	end process tb_counter_proc;

end architecture testbench;