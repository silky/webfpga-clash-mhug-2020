.PHONY: all

all: led1 led2

led1:
	./bin/synthesis.py fpga_top verilog/Led1/fpga_top/fpga_top.v pinmap.pcf

led2:
	./bin/synthesis.py fpga_top verilog/Led2/fpga_top/fpga_top.v pinmap.pcf

flash:
	webfpga flash bitstream.bin
