{-# OPTIONS_GHC -fglasgow-exts #-}

module Overlap where 

import Prelude hiding (foldr)
import Data.Foldable as F

import qualified Data.Vec as V
import Data.Vec ((:.)((:.)),Vec2D(Vec2D),Vec3,det,cramer'sRule,dot,snoc,Vec,N3)
import Data.List hiding (foldl',foldr)

-- Compute the area of overlap of two polygons, without computing the
-- intersection directly.

type Triangle = Vec3 Vec2D
tri a b c = a:.b:.c:.()

--                              | x0 y0 1 |
-- 2*signedArea of a triangle = | x0 y0 1 | 
--                              | x2 y2 1 |

signedArea2 :: Triangle -> Double
signedArea2 t = det (V.map (flip V.snoc 1 . V.unpack) t)
signedArea ::  Triangle -> Double
signedArea = (0.5*) . signedArea2

-- which side of line p0->p1 is x on?
orient :: Vec2D -> Vec2D -> Vec2D -> Double
orient p0 p1 x = signum $ signedArea (p0:.p1:.x:.())

-- line/line intersection. Solve system of line equations
isect :: Vec2D -> Vec2D -> Vec2D -> Vec2D -> Vec2D
isect p0 p1 t0 t1 = 
  let perp (Vec2D x y) = Vec2D (-y) x
      p = perp $ p1-p0
      t = perp $ t1-t0
      m = V.unpack p :. V.unpack t :. ()
      b = dot p p0 :. dot t t0 :. ()
  in  V.pack $ cramer'sRule m b

-- rotate the triangle vertices until the one with boolean tag == b' is first
rotateTil b' v@( x@(b,_) :. y :. z :. _ ) 
  |   b==b'   = v
  | otherwise = rotateTil b' (y:.z:.x:.())

-- triangle/line clip : 
triLineClip :: Vec2D -> Vec2D -> Vec2D -> Triangle -> ZOT Triangle
triLineClip p0 p1 p2 ts =
  let o = orient p0 p1 p2
      --os : tag vertices with inside/outside predicate
      os = V.map ((==o) . orient p0 p1) ts 
      --count : how many verts in positive half space?
      count = V.foldl (\s b -> if b then s+1 else s) 0 os :: Int
      --turn the triangle until the first vertex is the odd-one-out
      (t0:.t1:.t2:._) = V.map snd . rotateTil(count<2) . V.zipWith(,) os $ ts
      -- a,b : new vertices
      a = isect p0 p1 t0 t1
      b = isect p0 p1 t0 t2
  in  case count of 
        0 -> Zero
        1 -> One (tri t0 a b)
        2 -> Two (tri t1 b a) (tri t2 b t1)
        3 -> One (ts)

-- triangle/triangle area : overlap area between two triangles
triTriArea :: Triangle -> Triangle -> Double
triTriArea ta@(p0:.p1:.p2:._) tb =
  let l1 =             triLineClip p0 p1 p2   tb
      l2 =       fmap (triLineClip p1 p2 p0)  l1
      l3 = fmap (fmap (triLineClip p2 p0 p1)) l2 
  in  zot3Area l3


-- ZOT : Zero, One or Two
data ZOT a = Zero | One !a | Two !a !a

instance Functor ZOT where
  fmap f Zero      = Zero
  fmap f (One a)   = One (f a)
  fmap f (Two a b) = Two (f a) (f b)
  {-# INLINE fmap #-}

instance Foldable ZOT where
  foldr f z Zero      = z
  foldr f z (One a)   = f a z
  foldr f z (Two a b) = f b (f a z) 
  {-# INLINE foldr #-}


zot3Area :: ZOT (ZOT (ZOT Triangle)) -> Double
zot3Area = zsum . fmap (zsum . fmap (zsum . fmap signedArea))
zsum = foldr (+) 0


-- turn a list of vertices into a list of triangles.
triangles :: [Vec2D] -> [Triangle]
triangles [] = []
triangles (x:xs) = map (x:.) $ pairs xs
  where pairs (x:xs@(y:_)) = (x:.y:.()) : pairs xs
        pairs _ = []

-- compute overlap : for each pair of triangles, one from each polygon, sum the
-- the areas of intersection of the two triangles, multiplied by the product of
-- the signs. 
overlapArea ::  [Vec2D] -> [Vec2D] -> Double
overlapArea as bs = sum' $
    [ (sn a * sn b) * triTriArea a b | a <- triangles as, b <- triangles bs ]
  where sn = signum . signedArea      
        sum' = foldl' (+) 0

