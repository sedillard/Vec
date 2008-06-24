{- Copyright (c) 2008, Scott E. Dillard. All rights reserved. -}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-# HADDOCK_OPTIONS prune #-}

module Data.Vec.Base where

import Data.Vec.Nat

import Prelude hiding (map,zipWith,foldl,foldr,reverse,
                       take,drop,head,tail,sum,last,product,
                       minimum,maximum)
import qualified Prelude as P



-- | The vector constructor. @(:.)@ for vectors is like @(:)@ for lists, and
-- @()@ takes the place of @[]@. (The list of instances here is not meant to be
-- readable.)

data a :. b = !a :. !b
  deriving (Eq,Ord,Read)

infixr :.

--derived show outputs in prefix notation
instance (Show a, ShowVec v) => Show (a:.v) where
  show (a:.v) = "(" ++ show a ++ ":." ++ showVec v ++ ")"


-- | Helper to keep parentheses at bay. Just use @show@ as usual.
class ShowVec  v where
  showVec :: v -> String

instance ShowVec () where
  showVec = show
  {-# INLINE showVec #-}

instance (Show a, ShowVec v) => ShowVec (a:.v) where
  showVec (a:.v) = show a ++ ":." ++ showVec v
  {-# INLINE showVec #-}


-- * Vector Types
type Vec2  a = a :. a :. ()
type Vec3  a = a :. (Vec2 a)
type Vec4  a = a :. (Vec3 a)
type Vec5  a = a :. (Vec4 a)
type Vec6  a = a :. (Vec5 a)
type Vec7  a = a :. (Vec6 a)
type Vec8  a = a :. (Vec7 a)
type Vec9  a = a :. (Vec8 a)
type Vec10 a = a :. (Vec9 a)
type Vec11 a = a :. (Vec10 a)
type Vec12 a = a :. (Vec11 a)
type Vec13 a = a :. (Vec12 a)
type Vec14 a = a :. (Vec13 a)
type Vec15 a = a :. (Vec14 a)
type Vec16 a = a :. (Vec15 a)
type Vec17 a = a :. (Vec16 a)
type Vec18 a = a :. (Vec17 a)
type Vec19 a = a :. (Vec18 a)




-- | The type constraint @Vec n a v@ infers the vector type @v@ from the
-- length @n@, a type-level natural, and underlying component type @a@.  
-- So @x :: Vec N4 a v => v@ declares @x@ to be a 4-vector of @a@s.

class Vec n a v | n a -> v, v -> n a where
  -- | Make a uniform vector of a given length. @n@ is a type-level natural.
  -- Use `vec` when the length can be inferred.
  mkVec :: n -> a -> v

  -- | turn a list into a vector of inferred length
  fromList :: [a] -> v

  -- | get a vector element, which one is determined at runtime
  getElem :: Int -> v -> a

  -- | set a vector element, which one is determined at runtime
  setElem :: Int -> a -> v -> v

instance Vec N1 a ( a :. () ) where
  mkVec _ a = a :. ()
  fromList (a:_)   = a :. ()
  fromList []      = error "fromList: list too short"
  getElem !i (a :. _) 
    | i == 0    = a
    | otherwise = error "getElem: index out of bounds"
  setElem !i a _ 
    | i == 0    = a :. ()
    | otherwise = error "setElem: index out of bounds"
  {-# INLINE setElem #-}
  {-# INLINE getElem #-}
  {-# INLINE mkVec #-}
  {-# INLINE fromList #-}

instance Vec (Succ n) a (a':.v) => Vec (Succ (Succ n)) a (a:.a':.v) where
  mkVec _ a = a :. (mkVec undefined a)
  fromList (a:as)  = a :. (fromList as)
  fromList []      = error "fromList: list too short"
  getElem !i (a :. v)
    | i == 0    = a
    | otherwise = getElem (i-1) v
  setElem !i a (x :. v)
    | i == 0    = a :. v
    | otherwise = x :. (setElem (i-1) a v)
  {-# INLINE setElem #-}
  {-# INLINE getElem #-}
  {-# INLINE mkVec #-}
  {-# INLINE fromList #-}


-- | Make a uniform vector. The length is inferred.
vec ::  (Vec n a v) => a -> v
vec = mkVec undefined
{-# INLINE vec #-}


-- | get or set a vector element, known at compile
--time. Use the Nat types to access vector components. For instance, @get n0@
--gets the x component, @set n2 44@ sets the z component to 44. 


class Access n a v | v -> a where
  get  :: n -> v -> a
  set  :: n -> a -> v -> v

instance Access N0 a (a :. v) where
  get _ (a :. _) = a
  set _ a (_ :. v) = a :. v
  {-# INLINE set #-}
  {-# INLINE get #-}

instance Access n a v => Access (Succ n) a (a :. v) where
  get _ (_ :. v) = get (undefined::n) v
  set _ a' (a :. v) = a :. (set (undefined::n) a' v)
  {-# INLINE set #-}
  {-# INLINE get #-}


-- * List-like functions

-- | The first element.

class Head v a | v -> a  where 
  head :: v -> a

instance Head (a :. as) a where 
  head (a :. _) = a
  {-# INLINE head #-}


-- | All but the first element. 

class Tail v v_ | v -> v_ where 
  tail :: v -> v_

instance Tail (a :. as) as where 
  tail (_ :. as) = as
  {-# INLINE tail #-}




-- | Apply a function over each element in a vector. Constraint @Map a b u v@
-- states that @u@ is a vector of @a@s, @v@ is a vector of @b@s with the same
-- length as @u@, and the function is of type @a -> b@.

class Map a b u v | u -> a, v -> b, b u -> v, a v -> u where
  map :: (a -> b) -> u -> v

instance Map a b (a :. ()) (b :. ()) where
  map f (x :. ()) = (f x) :. ()
  {-# INLINE map #-}

instance Map a b (a':.u) (b':.v) => Map a b (a:.a':.u) (b:.b':.v) where
  map f (x:.v) = (f x):.(map f v)
  {-# INLINE map #-}




-- | Combine two vectors using a binary function. The length of the result is
-- the min of the lengths of the arguments. The constraint @ZipWith a b c u v
-- w@ states that @u@ is a vector of @a@s, @v@ is a vector of @b@s, @w@ is a
-- vector of @c@s, and the binary function is of type @a -> b -> c@.

class ZipWith a b c u v w | u->a, v->b, w->c, u v c -> w where
  zipWith :: (a -> b -> c) -> u -> v -> w

instance ZipWith a b c (a:.()) (b:.()) (c:.()) where
  zipWith f (x:._) (y:._) = f x y :.()
  {-# INLINE zipWith #-}

instance ZipWith a b c (a:.()) (b:.b:.bs) (c:.()) where
  zipWith f (x:._) (y:._) = f x y :.()
  {-# INLINE zipWith #-}

instance ZipWith a b c (a:.a:.as) (b:.()) (c:.()) where
  zipWith f (x:._) (y:._) = f x y :.()
  {-# INLINE zipWith #-}

instance 
  ZipWith a b c (a':.u) (b':.v) (c':.w) 
  => ZipWith a b c (a:.a':.u) (b:.b':.v) (c:.c':.w) 
    where
      zipWith f (x:.u) (y:.v) = f x y :. zipWith f u v
      {-# INLINE zipWith #-}


-- | Fold a function over a vector. 

class Fold a v | v -> a where
  fold  :: (a -> a -> a) -> v -> a
  foldl :: (b -> a -> b) -> b -> v -> b
  foldr :: (a -> b -> b) -> b -> v -> b

instance Fold a (a:.()) where
  fold  f   (a:._) = a 
  foldl f z (a:._) = (f $! z) $! a
  foldr f z (a:._) = (f $! a) $! z
  {-# INLINE fold #-}
  {-# INLINE foldl #-}
  {-# INLINE foldr #-}

instance Fold a (a':.u) => Fold a (a:.a':.u) where
  fold  f   (a:.v) = (f $! a) $! (fold f v)
  foldl f z (a:.v) = (f $! (foldl f z v)) $! a
  foldr f z (a:.v) = (f $! a) $! (foldr f z v)
  {-# INLINE fold #-}
  {-# INLINE foldl #-}
  {-# INLINE foldr #-}

-- | Reverse a vector 
reverse v = reverse' () v
{-# INLINE reverse #-}

-- Reverse helper function : builds the reversed list as its first argument
class Reverse' p v v' | p v -> v' where
  reverse' :: p -> v -> v'
  
instance Reverse' p () p where
  reverse' p () = p
  {-# INLINE reverse' #-}

instance Reverse' (a:.p) v v' => Reverse' p (a:.v) v' where
  reverse' p (a:.v) = reverse' (a:.p) v 
  {-# INLINE reverse' #-}


-- | Append two vectors 

class Append v1 v2 v3 | v1 v2 -> v3, v1 v3 -> v2 where 
  append :: v1 -> v2 -> v3

instance Append () v v where
  append _ = id
  {-# INLINE append #-}

instance Append (a:.()) v (a:.v) where
  append (a:.()) v = a:.v
  {-# INLINE append #-}

instance (Append (a':.v1) v2 v3) => Append (a:.a':.v1) v2 (a:.v3) where
  append (a:.u) v  =  a:.(append u v)
  {-# INLINE append #-}



-- | @take n v@ constructs a vector from the first @n@ elements of @v@. @n@ is a
-- type-level natural. For example @take n3 v@ makes a 3-vector of the first
-- three elements of @v@.

class Take n v v' | n v -> v', n v' -> v where
  take :: n -> v -> v'

instance Take N0 v () where
  take _ _ = ()
  {-# INLINE take #-}

instance Take n v v' => Take (Succ n) (a:.v) (a:.v') where
  take _ (a:.v) = a:.(take (undefined::n) v)
  {-# INLINE take #-}


-- | @drop n v@ strips the first @n@ elements from @v@. @n@ is a type-level
-- natural. For example @drop n2 v@ drops the first two elements.

class Drop n v v' | n v -> v', n v' -> v where
  drop :: n -> v -> v'
 
instance Drop N0 v v where
  drop _ = id
  {-# INLINE drop #-}

instance (Tail v' v'', Drop n v v') => Drop (Succ n) v v'' where
  drop _ = tail . drop (undefined::n)
  {-# INLINE drop #-}


-- | Get the last element, usually significant for some reason (quaternions,
-- homogenous coordinates, whatever)
class Last v a | v -> a where
  last :: v -> a

instance Last (a:.()) a where 
  last (a:._) = a
  {-# INLINE last #-}

instance Last (a':.v) a => Last (a:.a':.v) a where
  last (a:.v) = last v
  {-# INLINE last #-}

-- | @snoc v a@ appends the element a to the end of v. 

class Snoc v a v' | v a -> v', v' -> v a where 
  snoc :: v -> a -> v'

instance Snoc () a (a:.()) where
  snoc _ a = (a:.())
  {-# INLINE snoc #-}

instance Snoc v a (a:.v) => Snoc (a:.v) a (a:.a:.v) where
  snoc (b:.v) a = b:.(snoc v a)
  {-# INLINE snoc #-}



-- | sum of vector elements
sum ::  (Fold a v, Num a) => v -> a
sum x     = fold (+) x
{-# INLINE sum #-}

-- | product of vector elements
product ::  (Fold a v, Num a) => v -> a
product x = fold (*) x
{-# INLINE product #-}

-- | maximum vector element
maximum ::  (Fold a v, Ord a) => v -> a
maximum x = fold max x
{-# INLINE maximum #-}

-- | minimum vector element
minimum ::  (Fold a v, Ord a) => v -> a
minimum x = fold min x
{-# INLINE minimum #-}

toList ::  (Fold a v) => v -> [a]
toList = foldr (:) [] 
{-# INLINE toList #-}







-- * Matrix Types

type Mat22 a = Vec2 (Vec2 a)
type Mat23 a = Vec2 (Vec3 a)
type Mat24 a = Vec2 (Vec4 a)

type Mat32 a = Vec3 (Vec3 a)
type Mat33 a = Vec3 (Vec3 a)
type Mat34 a = Vec3 (Vec4 a)
type Mat35 a = Vec3 (Vec5 a)
type Mat36 a = Vec3 (Vec6 a)

type Mat42 a = Vec4 (Vec2 a)
type Mat43 a = Vec4 (Vec3 a)
type Mat44 a = Vec4 (Vec4 a)
type Mat45 a = Vec4 (Vec5 a)
type Mat46 a = Vec4 (Vec6 a)
type Mat47 a = Vec4 (Vec7 a)
type Mat48 a = Vec4 (Vec8 a)

-- | convert a matrix to a list-of-lists
matToLists ::  (Fold a v, Fold v m) => m -> [[a]]
matToLists   = (P.map toList) . toList
{-# INLINE matToLists   #-}

-- | convert a matrix to a list in row-major order
matToList  ::  (Fold a v, Fold v m) => m -> [a]
matToList    = concat . matToLists
{-# INLINE matToList    #-}

-- | convert a list-of-lists into a matrix
matFromLists :: (Vec j a v, Vec i v m) => [[a]] -> m
matFromLists = fromList . (P.map fromList)
{-# INLINE matFromLists #-}

-- | convert a list into a matrix. (row-major order)
matFromList :: forall i j v m a. (Vec i v m, Vec j a v, Nat i) => [a] -> m
matFromList  = matFromLists . groupsOf (nat(undefined::i))
  where groupsOf n xs = let (a,b) = splitAt n xs in a:(groupsOf n b)
{-# INLINE matFromList  #-}



