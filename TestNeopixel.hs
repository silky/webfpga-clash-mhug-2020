module TestNeopixel where

import qualified Prelude as Prelude
import Clash.Prelude
import Neopixel (neoT, State(..), initialState)

s1 = State { output = low, index = 24 , highCounter = 0 , lowCounter = 0 }

neo = flip neoT ()

-- Test in clash prelude like:
-- > mapM_ (putStrLn . show) $ (Prelude.take 4 test1)
test1 = Prelude.scanl (\(s, out) k -> neo s) (initialState, 0) [1..]
