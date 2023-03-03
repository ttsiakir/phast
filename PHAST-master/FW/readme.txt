PHAST processing FW review

Repo: PHAST
Last commit: 16th dec 2021, no parallel input data yet

compilation: all files added from src and xfft sim_netlist + tb. succesfully compiled. Simulation has been run

-- code review:

x 1. header not as of coding standard (take the header from any ces_util component, dont copy copyright notice from ces_util)
--------------------------------------------------------------------------------
-- (c) Copyright 2020 Campera Electronic Systems Srl. Via E. Mayer 69, Livorno
-- (Livorno), 57125, Italy. <www.campera-es.com>. All rights reserved.
-- THIS COPYRIGHT NOTICE MUST BE RETAINED AS PART OF THIS FILE AT ALL TIMES.
--------------------------------------------------------------------------------

x 2. remove textio from top level

3. the library shall not be work. use something like phast_lib or ces_phast_lib.

x 4. rand process proc_rand_gen shall be modified with wait until instead of wait for

x 5. istantiate modules in data flow order in the top level file

x 6.  active low signal sshall be named _n (e.g. rst_n_i)

x 7. clean the code from commented lines or dead code

8. We have to review the new version on 14/02/2022 

x 9. Use a FWFT FIFO before the fft

psa_submodules:
line n.     comment
x 1           ACA: insert header with description
x 22          luminosity data: ACA detail a bit more why is like that
x 96          the sum has to be done in a synchronous process, now its combinatorial and it would be slow
x 112	    the shifting constant should be a generic or a constant (not 7 bits). Also use the resize function of the numeric_std library
x 72          use s_shifted'length instead of s_shifted'high. This way you dont have to add two bits at the length
x 156	    use a delay module to generate avg_dv_o

psa:

x Make the module work for one pixel belonging in two subregions
x 44-55	descire better the configuration mechanism in the comments
x 442	I need 32 of the input selection modules. Each for every input coordinate.
x         I need to latch the data valids so that when all data valids have arrived I can send a burst. data valids can arrive at the same time.


windowing:

x 54	Use a generic to declare the output width
126 	Use a counter and delay modules instead of an fsm 

zero_padding:

x REMOVE THE fft_ready signal configuration and use a FWFT fifo instead
x 26 				use a different name like fr_length
x 34 				use a different name like fft_number_of_points


mem_interface:

85			use the enabl signal to read from the ping pong. We dont care if the first frame is invalid

QUESTIONS
 psa:
	Take care of s_hcnt and s_vcnt around reset
