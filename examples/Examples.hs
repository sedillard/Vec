{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Vec as V
import Prelude hiding (head)
import qualified Prelude as P 

import Foreign
import Control.Monad
import System

--Some sample vectors and matrices
--
--convenience types are defined :
--Vec2 a = a:.a:.()
--Vec3 a = a:.(Vec2 a)
--Vec4 a = ... ad infinitum. (Well, 19 anyway.)
--
--Mat33 a = Vec3 (Vec3 a)
-- ... and so on

v3  = 3:.2:.1:.() :: Vec3 Float

v3' = V.fromList [3,2,2] 
  --polymorphic, could be v2, v3. Trying to use it as v4
  --will cause pattern match error.

v3p = Vec3F 1 2 3 
  --packed matrix of unboxed floats. Use pack/unpack to covert between this
  --and Vec3 Float

v3'' = 0 --null vector, polymorphic
  --fromIntegral, fromRational and realToFrac all generate uniform vectors and
  --matrices from their arguments, but 'vec' is a shorter way to do this.

v3''' = vec 5 :: Vec3 Float -- a vector of all 5's

m3 = (1:.2:.3:.()):.
     (4:.5:.6:.()):.
     (7:.8:.10:.()):.() :: Mat33 Float

m3' = matFromLists [[1,2,3],[4,5,6],[7,8,10]] 
  -- polymorphic. Trying to make a matrix too big will cause pattern match
  -- error.

m3_ = matFromList [1,2,3,4,5,6,7,8,10] -- row-major

m3p = (Vec3F 1 2 3) :. (Vec3F 4 5 6) :. (Vec3F 7 8 10) :. ()
  --semi-packed matrix of unboxed floats

m3'' = 0 -- the null matrix, polymorphic. 
m3''' = identity -- the identity matrix, polymorphic, but must be square

m4 = (1:.2:.3:.4:.()):.
     (5:.6:.7:.7:.()):.
     (9:.10:.11:.12:.()):.
     (13:.13:.15:.16:.()):.() :: Mat44 Double

v4 = 4:.3:.2:.1:.() :: Vec4 Double


--vector arithmetic
t0 = v3 + v3' -- fixes type of v3' to Vec3 Float
t1 = normalize (v3-v3'') --fixes type of v3''
t2 = v3`cross`v3'

t3 = v3*v3' -- OH GOD NO WHAT IS THIS?!?! 
  --Num instance is meant to be practical, not elegant. This is component-wise
  --multiplcation.

t4 = V.sum(v3*v3') --dot product. (See it's not so bad)

--general list functions, map, fold, zipWith
lInfintiyNorm = V.fold max
boundingBox xs = (foldl1 (V.zipWith min) xs, foldl1 (V.zipWith max) xs)

--more list functions: head,tail,last,snoc,append,reverse,take,drop.


t5 = multmv m4 v4 --matrix * column-vector multiplication
t5' = multvm v4 m4 --row-vector * matrix multiplication
t6 = multmv m3' v3' 
  --does NOT fix type of m3'. Could be 2x3, 3x3, 100x3, etc., and result type
  --will vary accordingly. A type annotation is needed here, on either m3' or
  --t6.



t6' = multmv (m3'::Mat33 Float) v3' --now type of t6' is inferred as Vec3 Float

-- t6'' = multmv (1::Mat32 Float) v3' 
-- type error! Can't multiply a 3x2 matrix by a 3-vector

t6''' = multmv (1::Mat43 Float) v3' --t6''' is Vec4 Float

t7 = pack $ multmv (unpackMat m3p) (unpack v3p) 
  --bracketing expressions with pack/unpack generates excellent code.
  --unpackMat = V.map unpack

--get the determinant of a matrix
t8 = det m3

t9 = solve m4 v4 
  -- solve equation m4`multmv`x = v4. Maybe result, Nothing if no solution
t9' = cramer'sRule m3 v3 
  --same thing as solve, but closed form solution. Fast for 3x3. 
 
t10 = transpose m4
t11 = translate (V.map realToFrac v3) m4 
  -- if m4 is a projective matrix in row-major order,
  -- V.translate will apply a the vector v3 as a translation 

t12 = homVec   v3 :: Vec4 Float -- homogenous vector v3, 0 in last dimension
t13 = homPoint v3 :: Vec4 Float -- ... 1 in last dimension
t14 = project t12 -- divide xyz by w. No check for 0.

t15 = get n2 v3 -- get the 3rd element. 0-based indexing. 
t16 = nat n2  :: Int
  -- n2 is of type N2, a type-level natural. The value n2 is bottom. Use nat
  -- to get the Int. N0-through-N19 defined. N1 = Succ N0, N2 = Succ N1, and
  -- so forth.

t17 = getElem 2 v3 --get element using Ints, with bounds checking.

t18 = 0 :: (Num v, Vec N17 Float v) => v  
  -- 17-dimensional vector of Floats (but WHY?!)


-- Invert a lot of 4x4 matrices. Compile with
--
--  ghc --make Examples.hs -O2 -fvia-C -optc-O2 
--    -fexcess-precision -funfolding-use-threshold=999 
--      -funfolding-creation-threshold=999
--
-- Go get some coffee, then prepare to be almost impressed!
--
-- A C implementation, baseline.c, is provided with the library for
-- comparison. On my 2.16ghz Intel Core Duo, baseline 1000000 runs in 3.8sec,
-- and this program, compiled as above, runs in 5.3sec. But really we should
-- just import baseline.c using the ffi. (Boring)
--
-- For some reason, when this is installed as a library, it adds about a second
-- on to the above running time.
--
-- Simpler functions, like det, cramer'sRule, multmv, multmm, don't need
-- nearly as much optimization.  -O2 handles them just fine.


invert4d = packMat . fst . invertAndDet . unpackMat 

testLoop1 n =
  do
  a <- mallocArray n 
  b <- mallocArray n
  forM_ [0..n-1] $ \i -> pokeElemOff a i m4
  forM_ [0..n-1] $ \i -> 
    peekElemOff a i >>= pokeElemOff b i . fst . invertAndDet
    -- invertAndDet computes inverse and determinant at same time 
    -- Storable instances also generate tight code.
  peek b >>= print


main = testLoop1 =<< return . read . P.head =<< getArgs

