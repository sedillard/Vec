{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


import Data.Vec as V
import Prelude hiding (head)
import qualified Prelude as P 

import Foreign
import Control.Monad
import System

m3 = (1:.2:.3:.()):.
     (5:.6:.7:.()):.
     (13:.13:.15:.()):.() :: Mat33 Double

m4 = (1:.2:.3:.4:.()):.
     (5:.6:.7:.7:.()):.
     (9:.10:.11:.12:.()):.
     (13:.13:.15:.16:.()):.() :: Mat44 Double

v4 = 4:.3:.2:.1:.() :: Vec4 Double

multmm4d :: Mat44D -> Mat44D -> Mat44D 
multmm4d a b = packMat (multmm (unpackMat a) (unpackMat b))

--invertAndDet3d :: Mat33D -> (Mat33D,Double)
--invertAndDet3d = (\(m,d) -> (packMat m,d)) . invertAndDet . unpackMat

many n = 
  do
  a <- mallocArray n 
  b <- mallocArray n
  c <- mallocArray n
  forM_ [0..n-1] $ \i -> pokeElemOff a i m4
  forM_ [0..n-1] $ \i -> pokeElemOff b i m4
  forM_ [0..n-1] $ \i -> 
    peekElemOff a i >>= \a ->
    peekElemOff b i >>= \b ->
    pokeElemOff c i (multmm a b)
  peek c >>= print

main = 
  let x = many =<< return . read . P.head =<< getArgs
  in  sequence_ (replicate 3 x)



