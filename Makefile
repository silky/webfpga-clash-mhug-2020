.PHONY: nothing

nothing: 
	@echo Pick an option!

led1:
	clash --verilog Led1.hs && \
		./bin/synthesis.py fpga_top verilog/Led1/fpga_top/fpga_top.v pinmap.pcf

led2:
	clash --verilog Led2.hs && \
		./bin/synthesis.py fpga_top verilog/Led2/fpga_top/fpga_top.v pinmap.pcf

led3:
	clash --verilog Led3.hs && \
		./bin/synthesis.py fpga_top verilog/Led3/fpga_top/fpga_top.v pinmap.pcf

flash:
	webfpga flash bitstream.bin
