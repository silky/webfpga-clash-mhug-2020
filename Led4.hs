{-# LANGUAGE NumericUnderscores #-}

module Led4 where

import Clash.Prelude

-- Let's use state and a button to make adjustable
-- blinking.

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
  -> Signal System Bit  -- ^ Button
  -> Signal System Bool -- ^ LED (output)
topEntity clock reset' button =
  exposeClockResetEnable f clock reset enable i
  where
    -- Note: We've changed the input type of the button to be a "bit" not a
    -- bool. I don't know why this is necessary.
    f = mealy ledT (False, 0, BlinkSlow) . isRising 1
    
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
type State = (Bool, Unsigned 32, LedMode)

data LedMode
  = BlinkSlow
  | BlinkFast
  deriving (Eq, Generic, NFDataX)


toggleMode :: LedMode -> LedMode
toggleMode BlinkSlow = BlinkFast
toggleMode BlinkFast = BlinkSlow


ledT :: State
     -> Bool
     -> (State, Bool) 
ledT (ledState, counter, mode) buttonPressed = (nextState, ledState)
  where
    maxSlow   = 16_000_000 -- once per second
    maxFast   =  1_000_000 -- 16 times a second

    atMax 
      | mode == BlinkSlow = counter == maxSlow
      | mode == BlinkFast = counter == maxFast

    (mode', counterStart)
      | buttonPressed = ( toggleMode mode , 0       )
      | otherwise     = ( mode            , counter )

    nextState
      | atMax     = ( not ledState , 0                , mode' )
      | otherwise = ( ledState     , counterStart + 1 , mode' )
