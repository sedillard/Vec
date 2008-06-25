{-# OPTIONS_GHC -fglasgow-exts #-}

import qualified Data.Vec as V
import Data.Vec ((:.)((:.)),Vec2,Vec3,det,cramer'sRule,dot,snoc)
import Data.List as List

-- Compute the area of overlap of two polygons, without computing the
-- intersection directly.

type Triangle a = Vec3 (Vec2 a)

--                              | x0 y0 1 |
-- 2*signedArea of a triangle = | x0 y0 1 | 
--                              | x2 y2 1 |

signedArea2 t = det (V.map (flip V.snoc 1) t)
signedArea = (0.5*) . signedArea2

-- which side of line p0->p1 is x on?
orient p0 p1 x = signum $ signedArea (p0:.p1:.x:.())

-- line/line intersection. Solve system of line equations
isect p0 p1 t0 t1 = 
  let perp (x:.y:.()) = (-y):.x:.()
      p = perp $ p1-p0
      t = perp $ t1-t0
      m = p:.t:.()
      b = dot p p0 :. dot t t0 :. ()
  in  cramer'sRule m b

-- rotate the triangle vertices until the one with boolean tag == b' is first
rotateTil b' bxs@( bx@(b,x) :. bxs_ ) 
  |   b==b'   = bxs
  | otherwise = rotateTil b' (snoc bxs_ bx)

-- triangle/line clip : 
triLineClip p0 p1 p2 ts =
  let o = orient p0 p1 p2
      --os : tag vertices with inside/outside predicate
      os = V.map ((==o) . orient p0 p1) ts 
      --count : how many verts in positive half space?
      count = V.foldl (\s b -> if b then s+1 else s) 0 os
      --turn the triangle until the first vertex is the odd-one-out
      (t0:.t1:.t2:.()) = V.map snd . rotateTil (count<2) . V.zipWith (,) os $ ts
      -- a,b : new vertices
      a = isect p0 p1 t0 t1
      b = isect p0 p1 t0 t2
  in  case count of 
        0 -> []
        1 -> [(t0:.a:.b:.())]
        2 -> [(t1:.b:.a:.()),(t2:.b:.t1:.())]
        3 -> [ts] 

-- triangle/triangle clip : successive triangle/line clipping
triTriClip ta@(p0:.p1:.p2:.()) tb =
  [tb] >>= triLineClip p0 p1 p2 
       >>= triLineClip p1 p2 p0 
       >>= triLineClip p2 p0 p1

-- turn a list of vertices into a list of triangles.
triangles [] = []
triangles (x:xs) = map (x:.) $ pairs xs
  where pairs (x:xs@(y:_)) = (x:.y:.()) : pairs xs
        pairs _ = []

-- compute overlap : for each pair of triangles, one from each polygon, sum the
-- the areas of intersection of the two triangles, multiplied by the product of
-- the signs. 
overlapArea as bs = sum $
    [ (sn a * sn b) * (sum' . map (abs . signedArea) $ (triTriClip a b) )
    | a <- triangles as, b <- triangles bs ]
  where sn = signum . signedArea      
        sum' = List.foldl' (+) 0

-- some test cases
polygon1 = map V.fromList [[0,0],[4,0],[4,4],[2,4],[2,6],[0,6]] :: [Vec2 Double]
polygon2 = map V.fromList [[1,3],[3,3],[3,5],[1,5]] :: [Vec2 Double] --overlaps polygon1
polygon3 = map (+100) polygon2 --no overlap with polygon1
polygon4 = map ((\s -> s-2) . (*10)) polygon1 --fully encloses polygon1

main = 
  do
  print $ overlapArea polygon1 polygon1 -- =area of polygon1
  print $ overlapArea polygon1 polygon2
  print $ overlapArea polygon2 polygon1
  print $ overlapArea polygon1 polygon3
  print $ overlapArea polygon3 polygon1
  print $ overlapArea polygon1 polygon4
  print $ overlapArea polygon4 polygon1
  polygon1 <- return . reverse $ polygon1
  polygon2 <- return . reverse $ polygon2
  polygon3 <- return . reverse $ polygon3
  polygon4 <- return . reverse $ polygon4
  putStrLn "=="
  print $ overlapArea polygon1 polygon1 
  print $ overlapArea polygon1 polygon2
  print $ overlapArea polygon2 polygon1
  print $ overlapArea polygon1 polygon3
  print $ overlapArea polygon3 polygon1
  print $ overlapArea polygon1 polygon4
  print $ overlapArea polygon4 polygon1
