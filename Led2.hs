module Led2 where

import Clash.Prelude

-- Mealy-style

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
    f = mealy ledT ()
    
    -- Initial input
    i = button
    
    -- Always enabled
    enable = enableGen

    -- Clash assumes "high = reset"; i.e. don't run the
    -- program, but the webfpga board is "low = reset", 
    -- so this switches it.
    reset  = unsafeFromLowPolarity reset'


type State = ()

-- Mealy machine for our computing the LED state. 
ledT :: State
    -> Bool
    -> (State, Bool) 
ledT _ buttonState = ((), ledState)
  where
    ledState = buttonState
