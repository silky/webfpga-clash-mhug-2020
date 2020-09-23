# Simple examples of Clash for on the WebFPGA board

Examples are around the onboard LED and Neopixel.


### Requirements

- [Clash](https://clash-lang.org/) (tested on v1.2.4)

In order to deploy to the WebFPGA board, you can either use their
[WebIDE](https://webfpga.io/), or run the entire setup locally.

I've hacked together a working environment by using the [webfpga
cli](https://github.com/webfpga/cli.git) to flash, and running a
local hacked version of [icestorm
server](https://github.com/silky/icestorm-server), and copying "synthesis.py"
from that repo.

### Trying something

Open up `clash.clashi` and try loading something: `:l Led1.hs`

You can test generating verilog like: `:verilog`, and then the `Makefile` has
steps to actually generate the bistream and flash it, supposing you have the
device and the dependencies.
