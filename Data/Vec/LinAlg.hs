{- Copyright (c) 2008, Scott E. Dillard. All rights reserved. -}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_HADDOCK ignore-exports,prune #-}

module Data.Vec.LinAlg 
  (dot
  ,normSq
  ,norm
  ,normalize
  ,cross
  ,homPoint
  ,homVec
  ,project
  ,multvm
  ,multmv
  ,multmm
  ,translate
  ,column
  ,row
  ,Transpose(transpose)
  ,SetDiagonal(setDiagonal)
  ,GetDiagonal(getDiagonal)
  ,scale
  ,diagonal
  ,identity
  ,det
  ,cramer'sRule
  ,NearZero(nearZero)
  ,GaussElim(gaussElim)
  ,BackSubstitute(backSubstitute)
  ,BackSubstitute'(backSubstitute')
  ,invert
  ,invertAndDet
  ,solve
  ) where

import Prelude hiding (map,zipWith,foldl,foldr,reverse,take,drop,
                       head,tail,sum,length,last)
import qualified Prelude as P
import Data.Vec.Base
import Data.Vec.Nat
import Data.Vec.Instances

import Control.Monad
import Data.Maybe


-- | dot / inner / scalar product
dot ::  (Num a, Num v, Fold a v) => v -> v -> a
dot u v = sum (u*v)
{-# INLINE dot #-}

-- | vector norm, squared
normSq ::  (Num a, Num v, Fold a v) => v -> a
normSq v = dot v v
{-# INLINE normSq #-}

-- | vector / L2 / Euclidean norm
norm ::  (Num v, Floating a, Fold a v) => v -> a
norm v = sqrt (dot v v)
{-# INLINE norm #-}

-- | @normalize v@ is a unit vector in the direction of @v@. @v@ is assumed
-- non-null.
normalize :: (Floating a, Num v, Fold a v, Map a a v v) => v -> v
normalize v = map (/(norm v)) v
{-# INLINE normalize #-}

-- | 3d cross product.
cross :: Num a => Vec3 a -> Vec3 a -> Vec3 a
cross (ux:.uy:.uz:.()) (vx:.vy:.vz:.()) =
  (uy*vz-uz*vy):.(uz*vx-ux*vz):.(ux*vy-uy*vx):.()
{-# INLINE cross #-}

-- | lift a point into homogenous coordinates
homPoint ::  (Snoc v a v', Num a) => v -> v'
homPoint v = snoc v 1
{-# INLINE homPoint #-}

-- | point-at-infinity in homogenous coordinates
homVec ::  (Snoc v a v', Num a) => v -> v'
homVec   v = snoc v 0
{-# INLINE homVec   #-}

-- | project a vector from homogenous coordinates. Last vector element is
-- assumed non-zero.
project :: 
  ( Reverse' () t1 v'
  , Fractional t1
  , Vec a t t1
  , Reverse' () v (t :. t1)
  ) => v -> v'
project  v = case reverse v of (w:.u) -> reverse (u/vec w)
{-# INLINE project  #-}


-- | row vector * matrix
multvm :: 
  ( Transpose m mt
  , Map v a mt v'
  , Fold a v
  , Num a
  , Num v
  ) => v -> m -> v'
multvm v m = map (dot v) (transpose m)
{-# INLINE multvm #-}

-- | matrix * column vector
multmv :: 
  ( Map v a m v'
  , Num v
  , Fold a v
  , Num a
  ) => m -> v -> v'
multmv m v = map (dot v) m
{-# INLINE multmv #-}

-- | matrix * matrix 
multmm :: 
  (Map v v' m1 m3
  ,Map v a b v'
  ,Transpose m2 b
  ,Fold a v
  ,Num v
  ,Num a
  ) => m1 -> m2 -> m3
multmm a b = map (\v -> map (dot v) (transpose b)) a
{-# INLINE multmm #-}

-- | apply a translation to a projective transformation matrix
translate :: 
  (Transpose m mt
  ,Reverse' () mt (v' :. t)
  ,Reverse' (v' :. ()) t v'1
  ,Transpose v'1 m
  ,Num v'
  ,Num a
  ,Snoc v a v'
  ) => v -> m -> m
translate v m = 
  case reverse (transpose m) of
    (h:.t) -> transpose (reverse (((homVec v) + h) :. t))
{-# INLINE translate #-}

-- | get the @n@-th column as a vector. @n@ is a type-level natural.
column ::  (Transpose m mt, Access n v mt) => n -> m -> v
column n = get n . transpose 
{-# INLINE row #-}

-- | get the @n@-th row as a vector. @n@ is a type-level natural.
row ::  (Access n a v) => n -> v -> a
row n = get n
{-# INLINE column #-}


-- Matrix transpose wrapper class: infers type of one argument from the other,
-- because Transpose` can't do it, the fundeps there can't be bijective

-- | matrix transposition
class Transpose a b | a -> b, b -> a where 
  transpose :: a -> b

instance Transpose () () where
  transpose = id

instance 
    (Vec (Succ n) s (s:.ra)  --(s:ra) is an n-vector of s'es (row of a)
    ,Vec (Succ m) (s:.ra) ((s:.ra):.a)  --a is an m-vector of ra's
    ,Vec (Succ m) s (s:.rb)  --rb is an m-vector of s'es (row of b)
    ,Vec (Succ n) (s:.rb) ((s:.rb):.b)  --b is an n-vector of rb's
    ,Transpose' ((s:.ra):.a) ((s:.rb):.b)
    )
    => Transpose ((s:.ra):.a) ((s:.rb):.b)
  where
    transpose = transpose'
    {-# INLINE transpose #-}



class Transpose' a b | a->b
  where transpose' :: a -> b

instance Transpose' () () where 
  transpose' = id
  {-# INLINE transpose' #-}

instance 
    (Transpose' vs vs') => Transpose' ( () :. vs ) vs'
  where
    transpose' (():.vs) = transpose' vs
    {-# INLINE transpose' #-}

instance Transpose' ((x:.()):.()) ((x:.()):.()) where
  transpose' = id

instance 
    (Head xss_h xss_hh
    ,Map xss_h xss_hh (xss_h:.xss_t) xs'
    ,Tail xss_h xss_ht
    ,Map xss_h xss_ht (xss_h:.xss_t) xss_
    ,Transpose' (xs :. xss_) xss'
    )
    => Transpose' ((x:.xs):.(xss_h:.xss_t)) ((x:.xs'):.xss') 
  where
    transpose' ((x:.xs):.xss) =
      (x :. (map head xss)) :. (transpose' (xs :. (map tail xss) :: (xs:.xss_)))
    {-# INLINE transpose' #-}





class SetDiagonal v m | m -> v, v -> m where
  -- |set the diagonal of an n-by-n matrix to a given n-vector
  setDiagonal :: v -> m -> m

instance (Vec n a v, Vec n r m, SetDiagonal' N0 v m) => SetDiagonal v m where
  setDiagonal v m = setDiagonal' (undefined::N0) v m
  {-# INLINE setDiagonal #-}

class SetDiagonal' n v m  where
  setDiagonal' :: n -> v -> m -> m

instance SetDiagonal' n () m where
  setDiagonal' _ _ m = m
  {-# INLINE setDiagonal' #-}

instance 
    ( SetDiagonal' (Succ n) v m
    , Access n a r
    ) => SetDiagonal' n (a:.v) (r:.m) 
  where
    setDiagonal' _ (a:.v) (r:.m) = 
       (set (undefined::n) a r) :. (setDiagonal' (undefined::Succ n) v m)
    {-# INLINE setDiagonal' #-}



class GetDiagonal m v | m -> v, v -> m where
  -- |get the diagonal of an n-by-n matrix as a vector
  getDiagonal :: m -> v

instance (Vec n a v, Vec n v m, GetDiagonal' N0 () m v) => GetDiagonal m v where
  getDiagonal m = getDiagonal' (undefined::N0) () m
  {-# INLINE getDiagonal #-}

class GetDiagonal' n p m v where
  getDiagonal' :: n -> p -> m -> v

instance 
    (Access n a r
    ,Append p (a:.()) (a:.p)
    ) => GetDiagonal' n p (r:.()) (a:.p) 
  where
    getDiagonal' _ p (r:.()) = append p ((get (undefined::n) r) :. ())
    {-# INLINE getDiagonal' #-}

instance 
    (Access n a r
    ,Append p (a:.()) p'
    ,GetDiagonal' (Succ n) p' (r:.m) v
    ) 
    => GetDiagonal' n p (r:.r:.m) v
  where
    getDiagonal' _ p (r:.m) = 
      getDiagonal' (undefined::Succ n) (append p ((get (undefined::n) r):.())) m
    {-# INLINE getDiagonal' #-}


-- | @scale v m@ multiplies the diagonal of matrix @m@ by the vector @s@, component-wise. So
-- @scale 5 m@ multiplies the diagonal by 5, whereas @scale 2:.1 m@
-- only scales the x component.
scale :: 
  ( GetDiagonal' N0 () m r
  , Num r
  , Vec n a r
  , Vec n r m
  , SetDiagonal' N0 r m
  ) => r -> m -> m
scale s m = setDiagonal (s * (getDiagonal m)) m
{-# INLINE scale #-}


-- | @diagonal v@ is a square matrix with the vector v as the diagonal, and 0
-- elsewhere.
diagonal :: (Vec n a v, Vec n v m, SetDiagonal v m, Num m) => v -> m
diagonal v = setDiagonal v 0
{-# INLINE diagonal #-}


-- | identity matrix (square)
identity :: (Vec n a v, Vec n v m, Num v, Num m, SetDiagonal v m) => m
identity = diagonal 1 
{-# INLINE identity #-}


-- DropConsec: this is a helper function for computing determinants. Given an
-- n-vector v, drop each element from v and collect the remaning (n-1)-vectors
-- into an n-vector (ie an n-by-(n-1) matrix)
class DropConsec v vv | v -> vv where
  dropConsec :: v -> vv

instance 
  (Vec n a v
  ,Pred n n_
  ,Vec n_ a v_
  ,Vec n v_ vv
  ,DropConsec' () v vv
  ) => DropConsec v vv
  where
    dropConsec v = dropConsec' () v 
    {-# INLINE dropConsec #-}

class DropConsec' p v vv  where
  dropConsec' :: p -> v -> vv
    
instance DropConsec' p (a:.()) (p:.()) where
  dropConsec' p (a:.()) = (p:.())
  {-# INLINE dropConsec' #-}

instance 
    (Append p (a:.v) x
    ,Append p (a:.()) y
    ,DropConsec' y (a:.v) z
    ) 
    => DropConsec' p (a:.a:.v) (x:.z)
  where
    dropConsec' p (a:.v) = 
      (append p v) :. (dropConsec' (append p (a:.())) v)
    {-# INLINE dropConsec' #-}



--Alternating: vector of alternating positive/negative values. This is also a
--helper for computing determinants
class Alternating n a v | v -> n a where
  alternating :: n -> a -> v

instance Alternating N1 a (a:.()) where
  alternating _ !a = a:.()
  {-# INLINE alternating #-}

instance (Num a, Alternating n a (a:.v)) => Alternating (Succ n) a (a:.a:.v) where
  alternating _ !a = a:.(alternating (undefined::n) (negate $! a))
  {-# INLINE alternating #-}


-- The Determinant of a square matrix, by minor expansion. 
class Det' a m | m -> a where
  det' :: m -> a

instance Num a => Det' a ((a:.a:.()):.(a:.a:.()):.()) where
  det' ( (a:.b:.()) :. (c:.d:.()) :. () ) = a*d-b*c
  {-# INLINE det' #-}

--this instance is particularly ugly in order to avoid overlapping with the one above
instance
    (Num a
    ,Num (a:.a:.a:.v)
    ,Fold a (a:.a:.a:.v)
    ,Alternating (Succ (Succ (Succ n))) a (a:.a:.a:.v)
    ,DropConsec (a:.a:.a:.v) vv
    ,Map (a:.a:.a:.v) vv ((a:.a:.a:.v):.(a:.a:.a:.v):.m) vmt
    ,Transpose vmt vm
    ,Map ((a:.a:.v):.(a:.a:.v):.m_) a vm (a:.a:.a:.v)
    ,Det' a ((a:.a:.v):.(a:.a:.v):.m_)
    ,Vec (Succ (Succ (Succ n))) a (a:.a:.a:.v)
    ,Vec (Succ (Succ (Succ n))) (a:.a:.a:.v) ((a:.a:.a:.v):.(a:.a:.a:.v):.(a:.a:.a:.v):.m)
    )
     => 
    Det' a ((a:.a:.a:.v):.(a:.a:.a:.v):.(a:.a:.a:.v):.m)
  where
    det' (mh:.mt) =
      sum ((alternating undefined 1) * mh *
          (map det' (transpose (map dropConsec mt :: vmt))))
    {-# INLINE det' #-}


-- For now, use wrapper class to allow type inference. I think maybe the
-- squareness of the matrix is keeping Det' from inferring properly, so we'll
-- enforce that here. But really I have no clue.


-- | Determinant by minor expansion. Unfolds into a closed form expression.
-- This should be the fastest way for 4x4 and smaller, but @snd . gaussElim@
-- works too.

det :: forall n a r m. (Vec n a r, Vec n r m, Det' a m) => m -> a
det = det'
{-# INLINE det #-}



--ReplConsec : this is a helper for implementing Cramer's rule.  Given an
--n-vector v and a value r, replace each consecutive element from v with r,
--and collect the resulting n-vectors into an n-vector (ie an n-by-n matrix)

class ReplConsec a v vv | v->a, v->vv, vv->v, vv->a where
  replConsec :: a -> v -> vv

instance 
  (Vec n a v
  ,Vec n v vv
  ,ReplConsec' a () v vv
  ) => ReplConsec a v vv
  where
    replConsec a v = replConsec' a () v :: vv
    {-# INLINE replConsec #-}

class ReplConsec' a p v vv where
  replConsec' :: a -> p -> v -> vv

instance ReplConsec' a p () () where
  replConsec' _ _ () = ()
  {-# INLINE replConsec' #-}

instance 
    (Append p (a:.v) x
    ,Append p (a:.()) y
    ,ReplConsec' a y v z
    ) 
    => ReplConsec' a p (a:.v) (x:.z)
  where
    replConsec' r p (a:.v) = 
      (append p (r:.v)) :. (replConsec' r (append p (a :. ())) v)
    {-# INLINE replConsec' #-}




-- | @cramer'sRule m v@ computes the solution to @m\`multmv\`x=v@  using the
-- eponymous method. For larger than 3x3 you will want to use 'solve', which
-- uses 'gaussElim'. Cramer's rule, however, unfolds into a closed-form
-- expression, with no branches or allocations (other than the result). You may
-- need to increase the unfolding threshold to see this.

cramer'sRule :: 
  (Map a a1 b1 v
  ,Transpose w b1
  ,ZipWith a2 b vv v m w
  ,ReplConsec' a2 () b vv
  ,Vec n b vv
  ,Vec n a2 b
  ,Fractional a1
  ,Det' a1 m
  ,Det' a1 a
  ) => m -> v -> v
cramer'sRule m b =
  case map (\m' -> (det' m')/(det' m)) 
           (transpose (zipWith replConsec b m)) 
    of b' -> b' `asTypeOf` b 
{-# INLINE cramer'sRule #-}






mapFst f (a,b) = (f a,b)
{-# INLINE mapFst #-}


class Num a => NearZero a where
  -- | @nearZero x@ should be true when x is close enough to 0 to cause
  -- significant error in division. 
  nearZero :: a -> Bool
  nearZero 0 = True
  nearZero _ = False
  {-# INLINE nearZero #-}

instance NearZero Float where
  nearZero x = abs x < 1e-6
  {-# INLINE nearZero #-}

instance NearZero Double where
  nearZero x = abs x < 1e-14
  {-# INLINE nearZero #-}

instance NearZero Rational




-- Pivot1 : find a non-zero pivot column and put a 1 there. Second return
-- argument tracks value of determinant. Returns nothing if no pivot in the
-- first row. Does not try to find the 'best' pivot, only an acceptable one:
-- matrices are assumed small, roundoff error should be negligible. 

class Pivot1 a m where 
  pivot1 :: m -> Maybe (m,a)

--this instance prevents a fundep inferring type of a from m. 
instance Pivot1 a () where
  pivot1 _ = Nothing

instance 
    ( Fractional a, NearZero a
    ) => Pivot1 a ((a:.()):.()) 
  where
    pivot1 ((p:._):._) 
      | nearZero p = Nothing
      | otherwise  = Just (1,p)
    {-# INLINE pivot1 #-}

instance 
    ( Fractional a, NearZero a 
    , Map a a (a:.r) (a:.r)
    ) => Pivot1 a ((a:.(a:.r)):.()) 
  where
    pivot1 ((p:.r):._) 
      | nearZero p = Nothing
      | otherwise  = Just ((1 :. (map (/p) r)):.(), p)
    {-# INLINE pivot1 #-}

instance 
    ( Fractional a, NearZero a
    , Map a a (a:.r) (a:.r)
    , ZipWith a a a (a:.r) (a:.r) (a:.r) 
    , Map (a:.r) (a:.r) ((a:.r):.rs) ((a:.r):.rs)
    , Pivot1 a ((a:.r):.rs) 
    ) => Pivot1 a ((a:.r):.(a:.r):.rs) 
  where
    pivot1 (row@(p:._):.rows) 
      | nearZero p = pivot1 rows >>= \(r:.rs,p)-> Just(r:.row:.rs,p)
      | otherwise  = Just ( first:.(map add rows) , p)
          where first        = map (/p) row
                add r@(x:._) = zipWith (-) r . map (*x) $ first 
    {-# INLINE pivot1 #-}


-- Pivot : find a pivot. Second return argument tracks determinant.
-- Returns Nothing if no pivot anywhere.

class Pivot a m | m -> a where
  pivot :: m -> Maybe (m,a)

instance Pivot a (():.v) where
  pivot _ = Nothing
  {-# INLINE pivot #-}

instance 
    ( Fractional a
    , NearZero a
    , Pivot1 a rs 
    , Tail (a:.r) r
    , Map (a:.r) r ((a:.r):.rs) (r:.rs') 
    , Map r (a:.r) (r:.rs') ((a:.r):.rs)
    , Pivot1 a ((a:.r):.rs)
    , Pivot a (r:.rs')
    ) => Pivot a ((a:.r):.rs) 
  where
    pivot m = 
      mplus (pivot1 m) 
            (pivot (map tail m) >>= return . mapFst (map (0:.)) )
    {-# INLINE pivot #-}



-- | Gaussian elimination, adapted from Mirko Rahn:
-- <http://www.haskell.org/pipermail/glasgow-haskell-users/2007-May/012648.html>
--
-- This is more of a proof of concept. Using a foreign C function will run
-- slightly faster, and compile much faster. But where is the fun in that?
-- Set your unfolding threshold as high as possible.

class GaussElim a m | m -> a where
  -- | @gaussElim m@ returns a pair @(m',d)@ where @m'@ is @m@ in row echelon
  -- form and @d@ is the determinant of @m@. The determinant of @m'@ is 1 or 0,
  -- i.e., the leading coefficient of each non-zero row is 1.  
   
  gaussElim :: m -> (m,a)

instance (Num a, Pivot a (r:.())) => GaussElim a (r:.())
  where
    gaussElim m = fromMaybe (m,1) (pivot m) 
    {-# INLINE gaussElim #-}

instance 
    ( Fractional a
    , Map (a:.r) r ((a:.r):.rs) rs_
    , Map r (a:.r) rs_ ((a:.r):.rs) 
    , Pivot a ((a:.r):.(a:.r):.rs)
    , GaussElim a rs_
    ) => GaussElim a ((a:.r):.(a:.r):.rs)
  where
    gaussElim m =
      flip (maybe (m,1)) (pivot m) $ \(row:.rows,p) ->
        case gaussElim (map tail rows)
          of (rows',p') -> ( row:.(map (0:.) rows') , p*p')
    {-# INLINE gaussElim #-}



class BackSubstitute m where
  -- | backSubstitute takes a full rank matrix from row echelon form to reduced
  -- row echelon form. Returns @Nothing@ if the matrix is rank deficient. 
  backSubstitute :: m -> Maybe m 

instance BackSubstitute ((a:.r):.()) where
  backSubstitute = Just . id
  {-# INLINE backSubstitute #-}

instance 
    ( Map (a:.r) r ((a:.r):.rs) rs_ --map tail
    , Map r (a:.r) rs_ ((a:.r):.rs) --map cons
    , Fold (a,a:.r) aas
    , ZipWith a a a (a:.r) (a:.r) (a:.r)
    , Map a a (a:.r) (a:.r)
    , ZipWith a (a:.r) (a,a:.r) r ((a:.r):.rs) aas
    , Num a, NearZero a
    , BackSubstitute rs_
    ) => BackSubstitute ((a:.r):.(a:.r):.rs)
  where
    backSubstitute (r@(rh:.rt):.rs) 
      | nearZero (1-rh) = 
        liftM (map (0:.)) (backSubstitute . map tail $ rs) >>= \rs' -> 
          return . (:.rs') . foldl (\v (a,w) -> sub v a w) r $ 
            zipWith (,) rt rs'
      | otherwise = Nothing -- rank deficient
          where sub v a = zipWith (-) v . map (*a)
    {-# INLINE backSubstitute #-}




class BackSubstitute' m where
  -- | backSubstitute' takes a full rank matrix from row echelon form to reduced
  -- row echelon form. Returns garbage is matrix is rank deficient.
  backSubstitute' :: m -> m 

instance BackSubstitute' ((a:.r):.()) where
  backSubstitute' = id
  {-# INLINE backSubstitute' #-}

instance 
    ( Map (a:.r) r ((a:.r):.rs) rs_ --map tail
    , Map r (a:.r) rs_ ((a:.r):.rs) --map cons
    , Fold (a,a:.r) aas
    , ZipWith a a a (a:.r) (a:.r) (a:.r)
    , Map a a (a:.r) (a:.r)
    , ZipWith a (a:.r) (a,a:.r) r ((a:.r):.rs) aas
    , Num a
    , BackSubstitute' rs_
    ) => BackSubstitute' ((a:.r):.(a:.r):.rs)
  where
    backSubstitute' (r@(_:.rt):.rs) = 
      case map (0:.) (backSubstitute' . map tail $ rs) 
        of rs' -> (:.rs') $ foldl (\ v (a,w) -> sub v a w) r 
                              (zipWith (,) rt rs')
      where sub v a = zipWith (-) v . map (*a)
    {-# INLINE backSubstitute' #-}


-- | @invert m@ returns @Just@ the inverse of @m@ or @Nothing@ if @m@ is singular.
invert :: forall n a r m r' m'. 
  ( Num r, Num m
  , Vec n a r     -- r is row type
  , Vec n r m     -- m is matrix type
  , Append r r r' -- r' is a row of augmented matrix
  , ZipWith r r r' m m m' -- m' is the augmented matrix
  , Drop n r' r -- get the right half of an augmented matrix row
  , Map r' r m' m -- get the right half of the augmented matrix
  , SetDiagonal r m -- needed to make identity matrix
  , GaussElim a m'
  , BackSubstitute m'
  ) => m -> Maybe m
invert m = 
  return i >>= backSubstitute . fst . gaussElim . zipWith append m 
           >>= return . map dropn
  where dropn = drop (undefined::n)
        i = identity :: m
{-# INLINE invert #-}

-- | inverse and determinant. If det = 0, inverted matrix is garbage.
invertAndDet :: forall n a r m r' m'. 
  ( Num r, Num m
  , Vec n a r     -- r is row type
  , Vec n r m     -- m is matrix type
  , Append r r r' -- r' is a row of augmented matrix
  , ZipWith r r r' m m m' -- m' is the augmented matrix
  , Drop n r' r -- get the right half of an augmented matrix row
  , Map r' r m' m -- get the right half of the augmented matrix
  , SetDiagonal r m -- needed to make identity matrix
  , GaussElim a m'
  , BackSubstitute' m'
  ) => m -> (m,a)
invertAndDet m = 
  mapFst ( (map dropn) . backSubstitute') . gaussElim . zipWith append m $ i
  where dropn = drop (undefined::n)
        i = identity :: m
{-# INLINE invertAndDet #-}

-- | Solution of linear system by Gaussian elimination. Returns @Nothing@
-- if no solution. 
solve :: forall n a v r m r' m'. 
  ( Num r, Num m
  , Vec n a r     -- r is row type
  , Vec n r m     -- m is matrix type
  , Snoc r a r'   -- a row of the extended matrix is one longer
  , ZipWith r a r' m r m' -- m' is the augmented matrix
  , Drop n r' (a:.()) -- get the right part of an augmented matrix row
  , Map r' a m' r -- get the right part of the augmented matrix
  , GaussElim a m'
  , BackSubstitute m'
  ) => m -> r -> Maybe r
solve m v = 
  return v >>= backSubstitute . fst . gaussElim . zipWith snoc m 
           >>= return . map (head . drop (undefined::n)) 
{-# INLINE solve #-}

