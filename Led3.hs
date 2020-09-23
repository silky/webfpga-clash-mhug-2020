{-# LANGUAGE NumericUnderscores #-}

module Led3 where

import Clash.Prelude

-- Let's use state to blink every second.

{-# ANN topEntity
  (Synthesize
    { t_name   = "fpga_top"
    , t_inputs = [ PortName "WF_CLK"
                 , PortName "reset"
                 , PortName "WF_BUTTON"
                 ]
    , t_output = PortName "WF_LED"
    }) #-}
topEntity 
  :: Clock  System      -- ^ Clock
  -> Signal System Bool -- ^ Reset
  -> Signal System Bool -- ^ Button
  -> Signal System Bool -- ^ LED (output)
topEntity clock reset' button =
  exposeClockResetEnable f clock reset enable i
  where
    f = mealy ledT (False, 0)
    
    -- Initial input
    i = button
    
    -- Always enabled
    enable = enableGen

    -- Clash assumes "high = reset"; i.e. don't run the
    -- program, but the webfpga board is "low = reset", 
    -- so this switches it.
    reset  = unsafeFromLowPolarity reset'


-- Now our state tracks the ledState, and the 
-- blink counter.
type State = (Bool, Unsigned 32)


ledT :: State
     -> Bool
     -> (State, Bool) 
ledT (ledState, counter) buttonState = (nextState, ledState)
  where
    max   = 16_000_000 -- 16 MHz clock
    atMax = counter == max

    nextState
      | atMax     = ( not ledState , 0           )
      | otherwise = ( ledState     , counter + 1 )
