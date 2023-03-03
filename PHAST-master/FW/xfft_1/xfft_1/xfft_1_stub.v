// Copyright 1986-2019 Xilinx, Inc. All Rights Reserved.
// --------------------------------------------------------------------------------
// Tool Version: Vivado v.2019.1 (lin64) Build 2552052 Fri May 24 14:47:09 MDT 2019
// Date        : Mon Dec 20 12:53:49 2021
// Host        : pcw003x64 running 64-bit CentOS Linux release 7.9.2009 (Core)
// Command     : write_verilog -force -mode synth_stub
//               /home/a.campera/projects/phast/phast.runs/xfft_1_synth_1/xfft_1_stub.v
// Design      : xfft_1
// Purpose     : Stub declaration of top-level module interface
// Device      : xczu7ev-ffvc1156-2-e
// --------------------------------------------------------------------------------

// This empty module with port declaration file causes synthesis tools to infer a black box for IP.
// The synthesis directives are for Synopsys Synplify support to prevent IO buffer insertion.
// Please paste the declaration into a Verilog source file or add the file as an additional source.
(* x_core_info = "xfft_v9_1_2,Vivado 2019.1" *)
module xfft_1(aclk, aresetn, s_axis_config_tdata, 
  s_axis_config_tvalid, s_axis_config_tready, s_axis_data_tdata, s_axis_data_tvalid, 
  s_axis_data_tready, s_axis_data_tlast, m_axis_data_tdata, m_axis_data_tuser, 
  m_axis_data_tvalid, m_axis_data_tready, m_axis_data_tlast, m_axis_status_tdata, 
  m_axis_status_tvalid, m_axis_status_tready, event_frame_started, 
  event_tlast_unexpected, event_tlast_missing, event_fft_overflow, 
  event_status_channel_halt, event_data_in_channel_halt, event_data_out_channel_halt)
/* synthesis syn_black_box black_box_pad_pin="aclk,aresetn,s_axis_config_tdata[31:0],s_axis_config_tvalid,s_axis_config_tready,s_axis_data_tdata[31:0],s_axis_data_tvalid,s_axis_data_tready,s_axis_data_tlast,m_axis_data_tdata[31:0],m_axis_data_tuser[23:0],m_axis_data_tvalid,m_axis_data_tready,m_axis_data_tlast,m_axis_status_tdata[7:0],m_axis_status_tvalid,m_axis_status_tready,event_frame_started,event_tlast_unexpected,event_tlast_missing,event_fft_overflow,event_status_channel_halt,event_data_in_channel_halt,event_data_out_channel_halt" */;
  input aclk;
  input aresetn;
  input [31:0]s_axis_config_tdata;
  input s_axis_config_tvalid;
  output s_axis_config_tready;
  input [31:0]s_axis_data_tdata;
  input s_axis_data_tvalid;
  output s_axis_data_tready;
  input s_axis_data_tlast;
  output [31:0]m_axis_data_tdata;
  output [23:0]m_axis_data_tuser;
  output m_axis_data_tvalid;
  input m_axis_data_tready;
  output m_axis_data_tlast;
  output [7:0]m_axis_status_tdata;
  output m_axis_status_tvalid;
  input m_axis_status_tready;
  output event_frame_started;
  output event_tlast_unexpected;
  output event_tlast_missing;
  output event_fft_overflow;
  output event_status_channel_halt;
  output event_data_in_channel_halt;
  output event_data_out_channel_halt;
endmodule
