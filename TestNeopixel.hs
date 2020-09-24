module TestNeopixel where

import qualified Prelude as Prelude
import Clash.Prelude
import Neopixel (neoT, State(..), initialState, colourBits)

s1 = State { index = 24 , highCounter = 0 , lowCounter = 0, nextBit = Nothing }


neo = flip (neoT colourBits) ()

-- test1 = Prelude.drop 1 $ Prelude.scanl (\(s, out) k -> neo s) (initialState, 0) [1..]

-- Test in clash prelude like:
-- > mapM_ (putStrLn . show) $ (Prelude.take 4 test1)
test1 = Prelude.drop 1 $ Prelude.iterate (\(s, out) -> neo s) (initialState, 0)
