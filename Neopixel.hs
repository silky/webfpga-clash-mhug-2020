{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards    #-}

-- TODO:
--  - This doesn't seem to display inbetween colours. I'm not sure why? Maybe
--  it's a bug in my code somewhere?


-- Reference:
--  https://vivonomicon.com/2018/12/24/learning-how-to-fpga-with-neopixel-leds/

module Neopixel where

import qualified Prelude as Prelude
import Clash.Prelude
import Data.Bits
import Clash.Explicit.Testbench

-- Let's now push some colours to the Neopixel.

{-# ANN topEntity
  (Synthesize
    { t_name   = "fpga_top"
    , t_inputs = [ PortName "WF_CLK"
                 ]
    , t_output = PortName "WF_NEO"
    }) #-}
topEntity 
  :: Clock  System      -- ^ Clock
  -> Signal System Bit  -- ^ Neopixel (output)
topEntity clock =
  exposeClockResetEnable f clock reset enable i
  where
    f = mealy neoT initialState
    
    -- Initial input
    i = pure ()
    
    -- Always enabled
    enable = enableGen
    reset  = systemResetGen


type Colour = (Int, Int, Int)


colour :: Colour
colour = (0, 0, 10)


asBits :: Colour -> Vec 24 Bool
asBits (r, g, b) =
  bits g ++ bits r ++ bits b
  where
    bits p = zipWith testBit (repeat p) (iterate (SNat :: SNat 8) (+1) 0)


colourBits :: Vec 24 Bool
colourBits = asBits colour


-- (From: https://vivonomicon.com/2018/12/24/learning-how-to-fpga-with-neopixel-leds/)
--
-- > Getting back to the timing, these chips want to receive 24 bits of color
-- > for each LED in the strand; 8 green, 8 red, and 8 blue. Each bit is coded
-- > as a roughly 1.25-microsecond pulse between high/low signal levels. If the
-- > signal is high for ~350ns and low for ~900ns, it is a zero. If the signal
-- > is high for ~900ns and low for ~350ns, it is a one. Once an LED has
-- > received a full 24 bits of color on its input pin, it will “shift out” any
-- > subsequent bits on its output pin, acting like a serial shift register.
-- > This lets us send as many colors as we want ‘down a line’ of LEDs, and the
-- > last color sent will be shown on the first LED in the strand.

-- We have 1 Neopixel. So we need to send 24 bits for GRB in the following
-- way:
--
--  0 = High for 350 ns, low for 900 ns
--  1 = High for 900 ns, low for 350 ns

data State  = State
  { output      :: Bit         -- ^ Our output
  , index       :: Unsigned 32 -- ^ Index of next colour to send
  , highCounter :: Unsigned 32 -- ^ High counter
  , lowCounter  :: Unsigned 32 -- ^ Low counter
  } deriving (Show, Generic, NFDataX)


initialState :: State
initialState = State
  { output      = low
  , index       = 0
  , highCounter = 0
  , lowCounter  = 0
  }


neoT :: State
     -> ()
     -> (State, Bit) 
neoT (State {..}) _ = (nextState, out)
  where
    -- Clock is 16MHz 
    --    -> So after 16_000_000 ticks, 1 second has elapsed.
    --    -> 1 tick every 62.5 nanoseconds
    --
    -- 350 ns =  5.6 clock ticks =  6 ticks
    -- 900 ns = 14.5 clock ticks = 15 ticks
    -- total                     = 21 ticks

    stillColouring = index < 24
    bit            = colourBits !! min index 23

    (out, highCounter', lowCounter') = 
      if stillColouring
         then if bit
                  -- If it's a 1, we're high for 15 and low for 6
                  then if highCounter < 15
                            then (high , highCounter + 1 , lowCounter    )
                            else (low  , highCounter     , lowCounter + 1)
                  -- If it's a 0, we're high for 6 and low for 15
                  else if highCounter < 6
                            then (high , highCounter + 1 , lowCounter    )
                            else (low  , highCounter     , lowCounter + 1)
         else ( low, 0, 0 )


    -- Next bit if we've emitted enough
    (index', highCounter'', lowCounter'')
      | highCounter + lowCounter == 21
          = (index + 1, 0, 0)
      | otherwise
          = (index, highCounter', lowCounter')

    nextState = State
          { output      = out
          , index       = index'
          , highCounter = highCounter''
          , lowCounter  = lowCounter''
          }

--  Other ideas:
--
--  - Build a stream of colours that contains the right amount
--    of lows and highs? Trade-off between data storage?
--
--    Actually; can't do this because we need to know the size
--    of the vector in advance.
