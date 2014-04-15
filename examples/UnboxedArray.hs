
module UnboxedArray where

import Data.Vec hiding (map)
import Data.Vec.Packed
import Data.Array.Unboxed
import Data.List
import Data.Array.Base

import Control.Monad
import Control.Monad.ST



stuff :: UArray Int (Vec4 Double) -> UArray Int (Vec4 Double) -> UArray Int (Vec4 Double)
stuff a b =
  let (0,n) = bounds a in
  runST $
    do
    c <- newArray_ (0,n) :: ST s (STUArray s Int (Vec4 Double))
    forM_ [0..n] $ \i -> writeArray c i ((a!i)+(b!i))
    unsafeFreeze c
