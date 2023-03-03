library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use ieee.std_logic_textio.all;

library ces_util_lib;
use ces_util_lib.ces_util_pkg.all;

-----------------------------------------------------------

entity top_level is
port(avg_dv_o : out std_logic; avg_or_o : out std_logic);
end entity top_level;

-----------------------------------------------------------

architecture top_level_arch of top_level is

  -- Testbench DUT ports


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
  signal avg_o      : std_logic_vector(g_cam_data_w-1 downto 0);
  signal avg_dv     : std_logic;	  
  signal c : std_logic;


begin


  CLK_GEN : process
  begin
    clk_i <= '1';
    wait for 10 ns;
    clk_i <= '0';
    wait for 10 ns;
  end process CLK_GEN;

  RESET_GEN : process
  begin
    rst_i <= '0',
             '1' after 200 ns;
    wait;
  end process RESET_GEN;

  --PHAST_1_bd_wrapper_1 : entity work.PHAST_1_bd_wrapper
  --  port map (
  --    DDR_addr          => open,
  --    DDR_ba            => open,
  --    DDR_cas_n         => open,
  --    DDR_ck_n          => open,
  --    DDR_ck_p          => open,
  --    DDR_cke           => open,
  --    DDR_cs_n          => open,
  --    DDR_dm            => open,
  --    DDR_dq            => open,
  --    DDR_dqs_n         => open,
  --    DDR_dqs_p         => open,
  --    DDR_odt           => open,
  --    DDR_ras_n         => open,
  --    DDR_reset_n       => open,
  --    DDR_we_n          => open,
  --    FCLK_CLK0_0       => clk_i,
  --    FCLK_RESET0_N_0   => rst_i,
  --    FIXED_IO_ddr_vrn  => open,
  --    FIXED_IO_ddr_vrp  => open,
  --    FIXED_IO_mio      => open,
  --    FIXED_IO_ps_clk   => open,
  --    FIXED_IO_ps_porb  => open,
  --    FIXED_IO_ps_srstb => open,
  --    leds_4bits_tri_o  => open
  --  );

  psa_1 : entity work.psa
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
      avg_dv_o   => avg_dv_o
    );

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

    random_h_coor : entity ces_util_lib.ces_util_lfsr
		generic map (
			g_data_w => g_max_nop_per_subreg*f_ceil_log2(g_h_active_max)
		)
		port map (
			clk_i   => clk_i,
			rst_n_i => rst_i,
			load_i  => '0',
			seed_i  => "00000011100110000000001010000000100001000000001010100000001100000000000010000100000000001100000001110101",
			rng_o   => pnt_h_i
		);	

	random_v_coor : entity ces_util_lib.ces_util_lfsr
		generic map (
			g_data_w => g_max_nop_per_subreg*f_ceil_log2(g_v_active_max)
		)
		port map (
			clk_i   => clk_i,
			rst_n_i => rst_i,
			load_i  => '0',
			seed_i  => "00000001000100000000101011000000001011100000000101100000000110101000000010010100000000001100000000111100",
			rng_o   => pnt_v_i
		);	




    process(clk_i)
		begin
			if rising_edge(clk_i) then
				if rst_i='0' then
					pnt_nos_i <= (others => '0');
					pnt_dv_i <= '0';
					c <= '0';
				else
					if (pnt_nos_i="11111") then
						pnt_dv_i <= '0';
					else
						if c='0' then
							c <= '1';
							pnt_dv_i <= '1';
						else
							pnt_dv_i <= '1';
							pnt_nos_i <= std_logic_vector(unsigned(pnt_nos_i) + 1);
						end if;
					end if;
				end if;
			end if;
		end process;


end architecture top_level_arch;