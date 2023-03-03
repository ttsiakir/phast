--------------------------------------------------------------------------------
-- Title       : <Title Block>
-- Project     : Default Project Name
--------------------------------------------------------------------------------
-- File        : ces_video_hdmi_timing_tb.vhd
-- Author      : User Name <user.email@user.company.com>
-- Company     : User Company Name
-- Created     : Fri Oct 15 16:23:48 2021
-- Last update : Tue Nov  2 12:39:20 2021
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

-----------------------------------------------------------

entity ces_video_hdmi_timing_tb is

end entity ces_video_hdmi_timing_tb;

-----------------------------------------------------------

architecture testbench of ces_video_hdmi_timing_tb is

	-- Testbench DUT generics
	constant g_v_front  : natural   := 1;
	constant g_v_synch  : natural   := 0;
	constant g_v_back   : natural   := 0;
	constant g_v_active : natural   := 128;
	constant g_v_pol    : std_logic := '1';
	constant g_h_front  : natural   := 1;
	constant g_h_sync   : natural   := 0;
	constant g_h_back   : natural   := 0;
	constant g_h_active : natural   := 64;
	constant g_h_pol    : std_logic := '1';
	constant g_data_w   : integer := 24;

	--constant c_v_blank : natural := g_v_front + g_v_synch + g_v_back;
    --constant c_h_blank : natural := g_h_front + g_h_sync + g_h_back;
    --constant hcnt_max  : natural := c_h_blank + g_h_active;
    --constant vcnt_max  : natural := c_v_blank + g_v_active;

	-- Testbench DUT ports
	signal hdmi_clk_i   : std_logic;
	signal rst_i        : std_logic;
	signal hdmi_vact_o  : std_logic;
	signal hdmi_hact_o  : std_logic;
	signal hdmi_de_o    : std_logic;
	signal hdmi_vsync_o : std_logic;
	signal hdmi_hsync_o : std_logic;
	signal hdmi_tdata   : std_logic_vector(g_data_w-1 downto 0);


	--signal hcnt: unsigned(12 downto 0);
	--signal vcnt: unsigned(12 downto 0);

	-- Other constants
	constant C_CLK_PERIOD : real := 10.0e-9; -- NS
	
	--signal s_a : std_logic;

begin

	-----------------------------------------------------------
	-- Clocks and Reset
	-----------------------------------------------------------
	CLK_GEN : process
	begin
		hdmi_clk_i <= '1';
		wait for C_CLK_PERIOD / 2.0 * (1 SEC);
		hdmi_clk_i <= '0';
		wait for C_CLK_PERIOD / 2.0 * (1 SEC);
	end process CLK_GEN;

	RESET_GEN : process
	begin
		rst_i <= '1',
		         '0' after 20.0*C_CLK_PERIOD * (1 SEC);
		wait;
	end process RESET_GEN;

	-----------------------------------------------------------
	-- Testbench Stimulus
	-----------------------------------------------------------

	-----------------------------------------------------------
	-- Entity Under Test
	-----------------------------------------------------------
	DUT : entity work.ces_video_hdmi_timing
		generic map (
			g_v_front  => g_v_front,
			g_v_synch  => g_v_synch,
			g_v_back   => g_v_back,
			g_v_active => g_v_active,
			g_v_pol    => g_v_pol,
			g_h_front  => g_h_front,
			g_h_sync   => g_h_sync,
			g_h_back   => g_h_back,
			g_h_active => g_h_active,
			g_h_pol    => g_h_pol,
			g_data_w	=> g_data_w
		)
		port map (
			hdmi_clk_i   => hdmi_clk_i,
			rst_i        => rst_i,
			hdmi_vact_o  => hdmi_vact_o,
			hdmi_hact_o  => hdmi_hact_o,
			hdmi_de_o    => hdmi_de_o,
			hdmi_vsync_o => hdmi_vsync_o,
			hdmi_hsync_o => hdmi_hsync_o,
			hdmi_tdata   => hdmi_tdata
		);
--process to generate the counters
--		process(hdmi_clk_i)
--		begin
--			if rising_edge(hdmi_clk_i) then
--				if rst_i='1' then
--					hcnt<=(0=>'1',others=>'0');
--					vcnt<=(0=>'1',others=>'0');
--				else
--					if hcnt = hcnt_max then
--						hcnt<=(0=>'1',others=>'0');
--						if vcnt = vcnt_max then
--							vcnt<=(0=>'1',others=>'0');
--						else
--							vcnt<=vcnt+1;
--					end if;
--					else
--						hcnt<=hcnt+1;
--					end if;
--				end if;
--			end if;
--		end process;	 
--		
--		
--		process(hdmi_clk_i)				  
--			variable v_a : std_logic;
--		begin
--			if rising_edge(hdmi_clk_i) then
-- 
--				if hcnt = 1 then
--					v_a := '1';
--				else
--					v_a := '0';
--				end if;
--								s_a <= v_a;	 		
--			end if;
--		end process;

end architecture testbench;