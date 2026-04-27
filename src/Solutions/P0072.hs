module Solutions.P0072 where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Array.Base (STUArray, modifyArray', readArray)
import Data.Array.ST (newListArray)
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef)
import Data.Word (Word64)

solve :: Integer
solve = fromIntegral $ phiST 1000000

--------------------------------------------------------------------------------

phiST :: Word64 -> Word64
phiST limit = runST $ do
  phi <- newListArray (1, limit) [1 .. limit] :: ST s (STUArray s Word64 Word64)
  phiSum <- newSTRef 0 :: ST s (STRef s Word64)
  forM_ [2 .. limit] $ \i -> do
    phii' <- readArray phi i
    when (phii' == i) $ do
      let js = takeWhile (<= limit) [i, 2 * i ..]
      forM_ js $ \j -> modifyArray' phi j (\e -> e - e `div` i)
    phii <- readArray phi i
    modifySTRef' phiSum (+ phii)
  readSTRef phiSum
