module Led1 where

import Clash.Prelude

-- Simple-style

{-# ANN topEntity
  (Synthesize
    { t_name   = "fpga_top"
    , t_inputs = [PortName "WF_BUTTON"]
    , t_output = PortName "WF_LED"
    }) #-}
topEntity 
  :: Signal System Bool
  -> Signal System Bool
topEntity s = s
