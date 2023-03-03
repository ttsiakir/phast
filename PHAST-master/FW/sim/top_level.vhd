--==============================================================================
-- Module Name : ces_phast_top_level
-- Library     : ces_phast_lib
-- Project     : PHAST
-- Company     : Campera Electronic Systems Srl
-- Author      : T.Tsiakiris
--------------------------------------------------------------------------------
-- Description: This is a top level file of the PHAST project. It instantiates
--              the following components: Pixel selection and average (psa), 
--              windowing component, zero padding component, two Xilinx FFTs,
--              fft_ouput manager, memory interface. The purpose of this top 
--              level file is to receive frame data from 4 data channels and
--              perform averaging computations in pixel groups that are set by
--              the user. The averaged pixel luminosities are then processed by
--              two FFTs. The input-output widths as well as the burst duration
--              are configurable by generics.
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
use ieee.math_real.all;
library ces_util_lib;
use ces_util_lib.ces_util_pkg.all;
library ces_phast_lib;
-----------------------------------------------------------

entity ces_phast_top_level is
end entity ces_phast_top_level;

-----------------------------------------------------------

architecture top_level_arch of ces_phast_top_level is

	constant g_simulation : integer := 0;

	signal clk_i                       : STD_LOGIC;
	signal s_axis_config_tdata         : STD_LOGIC_VECTOR ( 15 downto 0 ) := (others => '0');
	signal s_axis_config_tvalid        : STD_LOGIC                        := '0';
	signal s_axis_config_tready        : STD_LOGIC;
	signal s_axis_data_tdata           : STD_LOGIC_VECTOR ( 31 downto 0 ) := (others => '0');
	signal s_axis_data_tvalid          : STD_LOGIC                        := '0';
	signal s_axis_data_tready          : STD_LOGIC                        := '0';
	signal s_axis_data_tlast           : STD_LOGIC                        := '0';
	signal m_axis_data_tdata           : STD_LOGIC_VECTOR ( 31 downto 0 );
	signal m_axis_data_tuser           : STD_LOGIC_VECTOR ( 15 downto 0 );
	signal m_axis_data_tvalid          : STD_LOGIC := '0';
	signal m_axis_data_tready          : STD_LOGIC := '0';
	signal m_axis_data_tlast           : STD_LOGIC;
	signal event_frame_started         : STD_LOGIC;
	signal event_tlast_unexpected      : STD_LOGIC;
	signal event_tlast_missing         : STD_LOGIC;
	signal event_fft_overflow          : STD_LOGIC;
	signal event_status_channel_halt   : STD_LOGIC;
	signal event_data_in_channel_halt  : STD_LOGIC;
	signal event_data_out_channel_halt : STD_LOGIC;

	constant C_CLK_PERIOD    : real    := 10.0e-9;
	constant g_max_nos       : integer := 32;
	constant g_burst_depth   : integer := 1024;
	constant g_ram_depth     : integer := 32*1024;
	constant g_ram_data_w    : integer := 32;
	constant g_win_inp_w     : integer := 8;
	constant g_win_table_w   : integer := 18;
	constant g_data_channels : integer := 4;
	constant g_h_active_max  : integer := 7680;
	constant g_v_active_max  : integer := 4320;
	constant g_h_active  : integer := 128;
	constant g_v_active  : integer := 64;

	signal rst_n_i      : std_logic;
	signal cam_nos_i    : std_logic_vector(f_ceil_log2(g_max_nos)-1 downto 0) := "10000";
	signal s_fft_tlast  : std_logic;
	signal data_i       : std_logic_vector(16-1 downto 0) := (others => '0');
	signal input_dv_i   : std_logic;
	signal s_zp_dv      : std_logic;
	signal s_zp_out     : std_logic_vector(16-1 downto 0);

	signal counter : integer := 0;

	signal s_ct_wen      : std_logic_vector(1 downto 0);
	signal s_ct_wr_addr  : std_logic_vector(f_ceil_log2(g_ram_depth) - 1 downto 0);
	signal s_ct_rd_addr  : std_logic_vector(f_ceil_log2(g_ram_depth) - 1 downto 0);
	signal s_ct_rd_dat   : std_logic_vector(g_ram_data_w - 1 downto 0);
	signal s_cr_wr_dat   : std_logic_vector(g_ram_data_w-1 downto 0);
	signal s_ct_rd_addr2 : std_logic_vector(f_ceil_log2(g_ram_depth) - 1 downto 0);
	signal s_ct_rd_dat2  : std_logic_vector(g_ram_data_w - 1 downto 0);

	signal win_w_ena_i   : std_logic                                             := '0';
	signal win_wr_addr_i : std_logic_vector(f_ceil_log2(g_max_nos) - 1 downto 0) := (others => '0');
	signal win_wr_dat_i  : std_logic_vector(g_win_table_w - 1 downto 0)          := (others => '0');
	signal win_o         : std_logic_vector(16-1 downto 0);
	signal win_dv_o      : std_logic;

	constant g_max_nop_per_subreg : integer := 8;

	signal pnt_h_i    : std_logic_vector(g_max_nop_per_subreg*f_ceil_log2(g_h_active_max)-1 downto 0) := (others => '0');
	signal pnt_v_i    : std_logic_vector(g_max_nop_per_subreg*f_ceil_log2(g_v_active_max)-1 downto 0) := (others => '0');
	signal pnt_nos_i  : std_logic_vector(f_ceil_log2(32)-1 downto 0)                                  := "00111";
	signal pnt_dv_i   : std_logic;
	signal fr_tdata_i : std_logic_vector(g_data_channels*g_win_inp_w-1 downto 0);
	signal fr_vact_i  : std_logic;
	signal fr_hact_i  : std_logic;
	signal fr_de_i    : std_logic;
	signal fr_vsync_i : std_logic;
	signal fr_hsync_i : std_logic;
	signal avg_o      : std_logic_vector((g_win_inp_w)-1 downto 0);
	signal avg_dv_o   : std_logic;


	signal ct_ren_uno_o  : std_logic;
	signal ct_ren_due_o  : std_logic;
	signal fft_ready_i   : std_logic := '1';
	signal s_mem_int_dat : std_logic_vector(2*16-1 downto 0);
	signal s_mem_int_dv  : std_logic;

	signal s_fifo_ren      : std_logic;
	signal s_fifo_empty    : std_logic;
	signal s_fifo_valid    : std_logic;

	signal win2_w_ena_i   : std_logic                                                 := '0';
	signal win2_wr_addr_i : std_logic_vector(f_ceil_log2(g_burst_depth) - 1 downto 0) := (others => '0');
	signal win2_wr_dat_i  : std_logic_vector(g_win_table_w - 1 downto 0)              := (others => '0');
	signal win2_o         : std_logic_vector(32-1 downto 0);
	signal win2_outp_dv   : std_logic;

	signal s_filter      : std_logic_vector(f_ceil_log2(g_burst_depth)-1 downto 0) := "1111111111";

	signal s_fft_tlast2_0 : std_logic;
	signal s_fft_tlast2_1 : std_logic;
	signal s_fifo2_ren    : std_logic;
	signal s_fifo2_empty  : std_logic;
	signal s_fifo2_valid  : std_logic;

	signal s_outp_ram_wen     : std_logic_vector(1 downto 0);
	signal s_outp_ram_wr_addr : std_logic_vector(f_ceil_log2(g_ram_depth)-1 downto 0);
	signal s_outp_ram_wr_dat  : std_logic_vector(32-1 downto 0);

	signal s_axis_config_tdata2         : STD_LOGIC_VECTOR ( 31 downto 0 ) := (others => '0');
	signal s_axis_config_tvalid2        : STD_LOGIC                        := '0';
	signal s_axis_config_tready2        : STD_LOGIC;
	signal s_axis_data_tdata2           : STD_LOGIC_VECTOR ( 31 downto 0 ) := (others => '0');
	signal s_axis_data_tvalid2          : STD_LOGIC                        := '0';
	signal s_axis_data_tready2          : STD_LOGIC                        := '0';
	signal s_axis_data_tlast2           : STD_LOGIC                        := '0';
	signal m_axis_data_tdata2           : STD_LOGIC_VECTOR ( 31 downto 0 );
	signal m_axis_data_tuser2           : STD_LOGIC_VECTOR ( 23 downto 0 );
	signal m_axis_data_tvalid2          : STD_LOGIC := '0';
	signal m_axis_data_tready2          : STD_LOGIC := '0';
	signal m_axis_data_tlast2           : STD_LOGIC;
	signal m_axis_status_tvalid2        : std_logic;
	signal m_axis_status_tready2        : std_logic;
	signal m_axis_status_tdata2         : std_logic_vector(7 downto 0);
	signal event_frame_started2         : STD_LOGIC;
	signal event_tlast_unexpected2      : STD_LOGIC;
	signal event_tlast_missing2         : STD_LOGIC;
	signal event_fft_overflow2          : STD_LOGIC;
	signal event_status_channel_halt2   : STD_LOGIC;
	signal event_data_in_channel_halt2  : STD_LOGIC;
	signal event_data_out_channel_halt2 : STD_LOGIC;

	COMPONENT xfft_0
		PORT (
			aclk                        : in  STD_LOGIC;
			aresetn                     : in  STD_LOGIC;
			s_axis_config_tdata         : in  STD_LOGIC_VECTOR ( 15 downto 0 );
			s_axis_config_tvalid        : in  STD_LOGIC;
			s_axis_config_tready        : out STD_LOGIC;
			s_axis_data_tdata           : in  STD_LOGIC_VECTOR ( 31 downto 0 );
			s_axis_data_tvalid          : in  STD_LOGIC;
			s_axis_data_tready          : out STD_LOGIC;
			s_axis_data_tlast           : in  STD_LOGIC;
			m_axis_data_tdata           : out STD_LOGIC_VECTOR ( 31 downto 0 );
			m_axis_data_tuser           : out STD_LOGIC_VECTOR ( 15 downto 0 );
			m_axis_data_tvalid          : out STD_LOGIC;
			m_axis_data_tready          : in  STD_LOGIC;
			m_axis_data_tlast           : out STD_LOGIC;
			m_axis_status_tdata         : out STD_LOGIC_VECTOR ( 7 downto 0 );
			m_axis_status_tvalid        : out STD_LOGIC;
			m_axis_status_tready        : in  STD_LOGIC;
			event_frame_started         : out STD_LOGIC;
			event_tlast_unexpected      : out STD_LOGIC;
			event_tlast_missing         : out STD_LOGIC;
			event_fft_overflow          : out STD_LOGIC;
			event_status_channel_halt   : out STD_LOGIC;
			event_data_in_channel_halt  : out STD_LOGIC;
			event_data_out_channel_halt : out STD_LOGIC
		);
	END COMPONENT;

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
		--psa_dv_i <= '0';
		rst_n_i <= '1';
		wait for 20.0*C_CLK_PERIOD*(1 SEC);
		rst_n_i <= '0';
		wait for 25.0*C_CLK_PERIOD*(1 SEC);
		rst_n_i <= '1';
		wait for 50.0*C_CLK_PERIOD*(1 SEC);
		wait;
	end process RESET_GEN;

	--counter for testbenching
	process(clk_i)
	begin
		if rising_edge(clk_i) then
			if rst_n_i='1' then
				counter <= counter + 1;
			else
				counter <= 0;
			end if;
		end if;
	end process;

	----------------------------------------------------------------------------
	--Components
	----------------------------------------------------------------------------
	ces_video_hdmi_timing_1 : entity work.ces_video_hdmi_timing
		generic map (
			g_v_front  => 4,
			g_v_synch  => 5,
			g_v_back   => 36,
			g_v_active => g_v_active,
			g_v_pol    => '1',
			g_h_front  => 10,
			g_h_sync   => 5,
			g_h_back   => 5,
			g_h_active => g_h_active/g_data_channels,
			g_h_pol    => '1',
			g_data_w   => g_win_inp_w,
			g_data_ch  => 4
		)
		port map (
			hdmi_clk_i   => clk_i,
			rst_i        => rst_n_i,
			hdmi_vact_o  => fr_vact_i,
			hdmi_hact_o  => fr_hact_i,
			hdmi_de_o    => fr_de_i,
			hdmi_vsync_o => fr_vsync_i,
			hdmi_hsync_o => fr_hsync_i,
			hdmi_tdata   => fr_tdata_i
		);

	--process to select random pixels from the frame
	proc_rand_gen : process
		variable seed1, seed2 : positive;
		variable rand         : real;
		variable int_rand     : integer;
		variable temp1         : std_logic_vector(g_max_nop_per_subreg*f_ceil_log2(g_h_active_max)-1 downto 0);
		variable temp2  : std_logic_vector(g_max_nop_per_subreg*f_ceil_log2(g_v_active_max)-1 downto 0);
	begin
		pnt_dv_i  <= '0';
		pnt_nos_i <= (others => '0');
		wait until rising_edge(rst_n_i);
		wait until clk_i='1';
		wait for C_CLK_PERIOD*(1 SEC);

		for k in 0 to 32-1 loop
			for j in 0 to 7 loop
				uniform(seed1,seed2,rand);
				int_rand:= integer(trunc(rand*real(g_h_active)));
				temp1((j+1)*f_ceil_log2(g_h_active_max)-1 downto j*f_ceil_log2(g_h_active_max)) := std_logic_vector(to_unsigned(int_rand, f_ceil_log2(g_h_active_max)));
			end loop;
			pnt_h_i <= temp1;
			wait for C_CLK_PERIOD*(1 SEC);
			for j in 0 to 7 loop
				uniform(seed1,seed2,rand);
				int_rand:= integer(trunc(rand*real(g_v_active)));
				temp2((j+1)*f_ceil_log2(g_v_active_max)-1 downto j*f_ceil_log2(g_v_active_max)) := std_logic_vector(to_unsigned(int_rand, f_ceil_log2(g_v_active_max)));
			end loop;
			pnt_v_i <= temp2(g_max_nop_per_subreg*f_ceil_log2(g_v_active_max)-1 downto 0);
			wait for C_CLK_PERIOD*(1 SEC);
			pnt_dv_i <= '1';
			wait for C_CLK_PERIOD*(1 SEC);
			pnt_dv_i  <= '0';
			pnt_nos_i <= std_logic_vector(unsigned(pnt_nos_i) + 1);
			wait for C_CLK_PERIOD*(1 SEC);
		end loop;
		wait;
	end process proc_rand_gen;

	psa_no_sim_gen : if g_simulation=0 generate
		psa_1 : entity ces_phast_lib.ces_phast_psa
			generic map (
				g_cam_data_w         => g_win_inp_w,
				g_max_nop_per_subreg => g_max_nop_per_subreg,
				g_data_channels      => g_data_channels,
				g_v_active_max       => g_v_active_max,
				g_h_active_max       => g_h_active_max
			)
			port map (
				clk_i      => clk_i,
				rst_n_i    => rst_n_i,
				pnt_h_i    => pnt_h_i,
				pnt_v_i    => pnt_v_i,
				pnt_nos_i  => pnt_nos_i,
				pnt_nop_i  => x"ABCF9873AAF8929492919AAB",
				pnt_dv_i   => pnt_dv_i,
				fr_tdata_i => fr_tdata_i,
				fr_vact_i  => fr_vact_i,
				fr_hact_i  => fr_hact_i,
				fr_de_i    => fr_de_i,
				fr_vsync_i => fr_vsync_i,
				fr_hsync_i => fr_hsync_i,
				avg_o      => avg_o,
				avg_dv_o   => avg_dv_o
			);
	end generate psa_no_sim_gen;

	--------------------------------------------------------------------------------
	--This part is used in simulation and simulates psa outputs
	--------------------------------------------------------------------------------
	avg_sim_gen : if g_simulation=1 generate
		random_avg : entity ces_util_lib.ces_util_lfsr
			generic map (
				g_data_w => g_win_inp_w
			)
			port map (
				clk_i   => clk_i,
				rst_n_i => rst_n_i,
				load_i  => '0',
				seed_i  => (others => '1'),
				rng_o   => avg_o
			);

		process
		begin
			wait until rising_edge(rst_n_i);
			wait for 450 ns;
			wait until clk_i='1';
			avg_dv_o <= '1';
			wait for 32.0*C_CLK_PERIOD*(1 SEC);
			wait until clk_i='1';
			avg_dv_o <= '0';
			while true loop
				wait for 2840 ns;
				wait until clk_i='1';
				avg_dv_o <= '1';
				wait for 32.0*C_CLK_PERIOD*(1 SEC);
				wait until clk_i='1';
				avg_dv_o <= '0';
			end loop;
			wait;
		end process;
	end generate avg_sim_gen;
	--------------------------------------------------------------------------------

	--windowing ram writing
	write_to_wind_0 : process
	begin
		wait until rising_edge(rst_n_i);
		wait for 100 ns;
		wait until clk_i='1';
		for k in 0 to 31 loop
			if k=0 then
				win_w_ena_i   <= '1';
				win_wr_addr_i <= (others => '0');
				win_wr_dat_i  <= std_logic_vector(to_unsigned(240298,18));
			else
				win_wr_addr_i <= std_logic_vector(unsigned(win_wr_addr_i)+1);
				win_wr_dat_i  <= std_logic_vector(unsigned(win_wr_dat_i)+8100);
			end if;
			wait until clk_i='1';
		end loop;
		win_w_ena_i <= '0';
		wait;
	end process write_to_wind_0;

	wind_0 : entity ces_phast_lib.ces_phast_wind
		generic map (
			g_data_w      => g_win_inp_w,
			g_burst_depth => g_max_nos,
			g_win_outp_w  => 16,
			g_win_table_w => g_win_table_w
		)
		port map (
			clk_i         => clk_i,
			rst_n_i       => rst_n_i,
			win_w_ena_i   => win_w_ena_i,
			win_wr_addr_i => win_wr_addr_i,
			win_wr_dat_i  => win_wr_dat_i,
			psa_avg_i     => avg_o,
			psa_dv_i      => avg_dv_o,
			win_o         => win_o,
			win_dv_o      => input_dv_i
		);

	data_i <= win_o;

	zero_padding_0 : entity ces_phast_lib.ces_phast_zero_padding
		generic map (
			g_burst_depth => g_max_nos,
			g_data_w      => 16
		)
		port map (
			clk_i       => clk_i,
			rst_n_i     => rst_n_i,
			fr_nos_i    => cam_nos_i,
			data_i      => data_i,
			input_dv_i  => input_dv_i,
			fft_tlast_o => s_fft_tlast,
			fifo_data_o => s_zp_out,
			fifo_wen_o  => s_zp_dv
		);

	fifo_before_fft : entity ces_util_lib.ces_util_fifo
		generic map (
			g_dual_clock         => false,
			g_fwft               => true,
			g_wr_depth           => 32,
			g_wr_data_w          => 16+1,
			g_rd_data_w          => 16+1,
			g_rd_latency         => 1,
			g_ren_ctrl           => false,
			g_wen_ctrl           => false,
			g_almost_empty_limit => 5,
			g_almost_full_limit  => 31,
			g_sanity_check       => true,
			g_simulation         => 1
		)
		port map (
			wr_clk_i            => clk_i,
			rd_clk_i            => clk_i,
			wr_rst_n_i          => rst_n_i,
			rd_rst_n_i          => rst_n_i,
			din_i(16 downto 1)  => s_zp_out,
			din_i(0)            => s_fft_tlast,
			wen_i               => s_zp_dv,
			full_o              => open,
			almost_full_o       => open,
			dout_o(16 downto 1) => s_axis_data_tdata(32-1 downto 16),
			dout_o(0)           => s_axis_data_tlast,
			ren_i               => s_fifo_ren,
			empty_o             => s_fifo_empty,
			almost_empty_o      => open,
			valid_o             => s_fifo_valid
		);
	s_fifo_ren                       <= s_axis_data_tready and (not s_fifo_empty);
	s_axis_data_tvalid               <= s_fifo_valid and s_axis_data_tready;
	s_axis_data_tdata(16-1 downto 0) <= (others => '0');

	fft : xfft_0
		PORT MAP (
			aclk                        => clk_i,
			aresetn                     => rst_n_i,
			s_axis_config_tdata         => s_axis_config_tdata,
			s_axis_config_tvalid        => s_axis_config_tvalid,
			s_axis_config_tready        => s_axis_config_tready,
			s_axis_data_tdata           => s_axis_data_tdata,
			s_axis_data_tvalid          => s_axis_data_tvalid,
			s_axis_data_tready          => s_axis_data_tready,
			s_axis_data_tlast           => s_axis_data_tlast,
			m_axis_data_tdata           => m_axis_data_tdata,
			m_axis_data_tuser           => m_axis_data_tuser,
			m_axis_data_tvalid          => m_axis_data_tvalid,
			m_axis_data_tready          => m_axis_data_tready,
			m_axis_data_tlast           => m_axis_data_tlast,
			m_axis_status_tdata         => open,
			m_axis_status_tvalid        => open,
			m_axis_status_tready        => '1',
			event_frame_started         => event_frame_started,
			event_tlast_unexpected      => event_tlast_unexpected,
			event_tlast_missing         => event_tlast_missing,
			event_fft_overflow          => event_fft_overflow,
			event_status_channel_halt   => event_status_channel_halt,
			event_data_in_channel_halt  => event_data_in_channel_halt,
			event_data_out_channel_halt => event_data_out_channel_halt
		);


	--fft0 output signals manipulator
	fft_out_mng_1 : entity ces_phast_lib.ces_phast_fft_out_mng
		generic map (
			g_max_nos   => g_max_nos,
			g_ram_depth => g_ram_depth
		)
		port map (
			clk_i              => clk_i,
			rst_n_i            => rst_n_i,
			non_fil_fr_i       => s_filter,
			axis_data_tdata_i  => m_axis_data_tdata,
			axis_data_tuser_i  => m_axis_data_tuser,
			axis_data_tvalid_i => m_axis_data_tvalid,
			axis_data_tready_o => m_axis_data_tready,
			axis_data_tlast_i  => m_axis_data_tlast,
			ct_wen_o           => s_ct_wen,
			ct_wr_addr_o       => s_ct_wr_addr,
			ct_wr_data_o       => s_cr_wr_dat
		);


	--Corner Turner BRAMs

	--corner turner uno
	corner_turner_uno : entity ces_util_lib.ces_util_ram_r_w
		generic map (
			g_ram_latency => 2,
			g_ram_data_w  => 2*16,
			g_ram_depth   => g_ram_depth,
			g_init_file   => "",
			g_simulation  => 1
		)
		port map (
			clk_i     => clk_i,
			ena_i     => '1',
			wen_i     => s_ct_wen(0),
			wr_addr_i => s_ct_wr_addr,
			wr_dat_i  => s_cr_wr_dat,
			enb_i     => ct_ren_uno_o,
			rd_addr_i => s_ct_rd_addr,
			rd_dat_o  => s_ct_rd_dat
		);

	--corner turner due
	corner_turner_due : entity ces_util_lib.ces_util_ram_r_w
		generic map (
			g_ram_latency => 2,
			g_ram_data_w  => 2*16,
			g_ram_depth   => g_ram_depth,
			g_init_file   => "",
			g_simulation  => 1
		)
		port map (
			clk_i     => clk_i,
			ena_i     => '1',
			wen_i     => s_ct_wen(1),
			wr_addr_i => s_ct_wr_addr,
			wr_dat_i  => s_cr_wr_dat,
			enb_i     => ct_ren_due_o,
			rd_addr_i => s_ct_rd_addr2,
			rd_dat_o  => s_ct_rd_dat2
		);

	-------------After Corner Turners-------------

	mem_int_1 : entity ces_phast_lib.ces_phast_mem_int
		generic map (
			g_ct_depth => g_ram_depth
		)
		port map (
			clk_i            => clk_i,
			rst_n_i          => rst_n_i,
			non_fil_fr_i     => s_filter,
			ct_rd_addr_uno_o => s_ct_rd_addr,
			ct_rd_dat_uno_i  => s_ct_rd_dat,
			ct_rd_addr_due_o => s_ct_rd_addr2,
			ct_rd_dat_due_i  => s_ct_rd_dat2,
			ct_ren_uno_o     => ct_ren_uno_o,
			ct_ren_due_o     => ct_ren_due_o,
			ct_wr_addr_i     => s_ct_wr_addr,
			ct_wen_i         => s_ct_wen,
			fft_ready_i      => s_axis_data_tready2,
			fft_tlast_o      => s_fft_tlast2_0,
			win_dat_o        => s_mem_int_dat,
			win_dv_o         => s_mem_int_dv
		);

	--delay to synch fft tlast input with windowing component output.
	fft_1_tlast_delay_gen : entity ces_util_lib.ces_util_delay
		generic map (
			g_delay  => 6,
			g_data_w => 1
		)
		port map (
			clk_i     => clk_i,
			ce_i      => '1',
			din_i(0)  => s_fft_tlast2_0,
			dout_o(0) => s_fft_tlast2_1
		);

	write_to_wind_1 : process
	begin
		wait until rising_edge(rst_n_i);
		wait for 100 ns;
		wait until clk_i='1';
		for k in 0 to 1024-1 loop
			if k=0 then
				win2_w_ena_i   <= '1';
				win2_wr_addr_i <= (others => '0');
				win2_wr_dat_i  <= std_logic_vector(to_unsigned(176194,18));
			else
				win2_wr_addr_i <= std_logic_vector(unsigned(win2_wr_addr_i)+1);
				win2_wr_dat_i  <= std_logic_vector(unsigned(win2_wr_dat_i)+69);
			end if;
			wait until clk_i='1';
		end loop;
		win2_w_ena_i <= '0';
		wait;
	end process write_to_wind_1;

	wind_ct : entity ces_phast_lib.ces_phast_wind_ct
		generic map (
			g_data_w      => 32,
			g_burst_depth => 1024,
			g_win_outp_w  => 32,
			g_win_table_w => g_win_table_w
		)
		port map (
			clk_i         => clk_i,
			rst_n_i       => rst_n_i,
			win_w_ena_i   => win2_w_ena_i,
			win_wr_addr_i => win2_wr_addr_i,
			win_wr_dat_i  => win2_wr_dat_i,
			psa_avg_i     => s_mem_int_dat,
			psa_dv_i      => s_mem_int_dv,
			win_o         => win2_o,
			win_dv_o      => win2_outp_dv
		);

	fifo_before_second_fft : entity ces_util_lib.ces_util_fifo
		generic map (
			g_dual_clock         => false,
			g_fwft               => true,
			g_wr_depth           => 1024,
			g_wr_data_w          => 2*16+1,
			g_rd_data_w          => 2*16+1,
			g_rd_latency         => 1,
			g_ren_ctrl           => false,
			g_wen_ctrl           => false,
			g_almost_empty_limit => 5,
			g_almost_full_limit  => 1020,
			g_sanity_check       => true,
			g_simulation         => 1
		)
		port map (
			wr_clk_i            => clk_i,
			rd_clk_i            => clk_i,
			wr_rst_n_i          => rst_n_i,
			rd_rst_n_i          => rst_n_i,
			din_i(32 downto 1)  => win2_o,
			din_i(0)            => s_fft_tlast2_1,
			wen_i               => win2_outp_dv,
			full_o              => open,
			almost_full_o       => open,
			dout_o(32 downto 1) => s_axis_data_tdata2,
			dout_o(0)           => s_axis_data_tlast2,
			ren_i               => s_fifo2_ren,
			empty_o             => s_fifo2_empty,
			almost_empty_o      => open,
			valid_o             => s_fifo2_valid
		);
	s_fifo2_ren         <= s_axis_data_tready2 and (not s_fifo2_empty);
	s_axis_data_tvalid2 <= s_fifo2_valid and s_axis_data_tready2;

	fft_1 : entity work.xfft_1
		port map (
			aclk                        => clk_i,
			aresetn                     => rst_n_i,
			s_axis_config_tvalid        => s_axis_config_tvalid2,
			s_axis_config_tready        => s_axis_config_tready2,
			s_axis_config_tdata         => s_axis_config_tdata2,
			s_axis_data_tvalid          => s_axis_data_tvalid2,
			s_axis_data_tready          => s_axis_data_tready2,
			s_axis_data_tdata           => s_axis_data_tdata2,
			s_axis_data_tlast           => s_axis_data_tlast2,
			m_axis_data_tvalid          => m_axis_data_tvalid2,
			m_axis_data_tready          => m_axis_data_tready2,
			m_axis_data_tdata           => m_axis_data_tdata2,
			m_axis_data_tuser           => m_axis_data_tuser2,
			m_axis_data_tlast           => m_axis_data_tlast2,
			m_axis_status_tvalid        => m_axis_status_tvalid2,
			m_axis_status_tready        => m_axis_status_tready2,
			m_axis_status_tdata         => m_axis_status_tdata2,
			event_frame_started         => event_frame_started2,
			event_tlast_unexpected      => event_tlast_unexpected2,
			event_tlast_missing         => event_tlast_missing2,
			event_fft_overflow          => event_fft_overflow2,
			event_status_channel_halt   => event_status_channel_halt2,
			event_data_in_channel_halt  => event_data_in_channel_halt2,
			event_data_out_channel_halt => event_data_out_channel_halt2
		);


	m_axis_status_tready2 <= '1';


	fft_out_mng_2 : entity ces_phast_lib.ces_phast_fft_out_mng
		generic map (
			g_max_nos   => 1024,
			g_ram_depth => 32*1024
		)
		port map (
			clk_i              => clk_i,
			rst_n_i            => rst_n_i,
			non_fil_fr_i       => s_filter,
			axis_data_tdata_i  => m_axis_data_tdata2,
			axis_data_tuser_i  => m_axis_data_tuser2(16-1 downto 0),
			axis_data_tvalid_i => m_axis_data_tvalid2,
			axis_data_tready_o => m_axis_data_tready2,
			axis_data_tlast_i  => m_axis_data_tlast2,
			ct_wen_o           => s_outp_ram_wen,
			ct_wr_addr_o       => s_outp_ram_wr_addr,
			ct_wr_data_o       => s_outp_ram_wr_dat
		);



	output_memory : entity ces_util_lib.ces_util_ram_r_w
		generic map (
			g_ram_latency => 2,
			g_ram_data_w  => 2*16,
			g_ram_depth   => g_ram_depth,
			g_init_file   => "",
			g_simulation  => 1
		)
		port map (
			clk_i     => clk_i,
			ena_i     => '1',
			wen_i     => f_vector_or(s_outp_ram_wen),
			wr_addr_i => s_outp_ram_wr_addr,
			wr_dat_i  => s_outp_ram_wr_dat,
			enb_i     => '1',
			rd_addr_i => (others => '0'),
			rd_dat_o  => open
		);
end architecture top_level_arch;