{- Copyright (c) 2008, Scott E. Dillard. All rights reserved. -}

{-# LANGUAGE CPP                       #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}

-- | Packed vectors : use these whenever possible. The generic vector type is
-- is represented at run-time by a linked list of boxed values. Packed types,
-- however, store the vector components sequentially in memory. Vector
-- operations can be defined using the generic types, and the compiler will
-- inline and specialize these definitions for the packed types, avoiding any
-- list cells or unnecessary heap allocations.
--
-- Packed vectors are related to their unpacked representations by way of an
-- associated type. An instance of class @'PackedVec' v@ declares that @v@ has a
-- packed representation, and the type of that is @'Packed' v@. The packed
-- constructors are named @Vec@/NT/ where /N/ is 2, 3 or 4 and /T/ is @I@, @F@
-- or @D@ for @Int@, @Float@ or @Double@. So the expression @Vec3D x y z@
-- constructs a packed 3-vector of Doubles, the type of which is @Packed (Vec3
-- Double)@.  The constructor name is also a synonym for the packed type name,
-- i.e., @type Vec3D = Packed (Vec3 Double)@, so the packed type acts as if it
-- had been declared @data Vec3D = Vec3D x y z@.
-- 
-- Storable, Num, Fractional, Fold, Map, and ZipWith instances are provided
-- for packed vectors, so some operations do not require pack/unpack. For
-- example, @'dot'@ does not require pack/unpack because it is defined in
-- terms of @'zipWith'@ and @'fold'@. However @'transpose'@, @'det'@,
-- @'gaussElim'@ and most others are recursive, and so you'll still need to
-- use pack/unpack with these. This goes for @'multmm'@ as well because it
-- uses @'transpose'@. Some functions, like @'multmv'@, do not need their
-- arguments to be unpacked, but the result is a polymorphic vector @(:.)@, so
-- you will need to pack it again. I admit that this is awkward. 

module Data.Vec.Packed where

import Prelude hiding (map,foldl,foldr,zipWith)
import Data.Vec.Base as V
import Data.Vec.LinAlg --just for haddock
import Data.Vec.Instances
import Data.Word
import Data.Int
import Foreign
import Test.QuickCheck

-- | PackedVec class : relates a vector type to its space-optimized
-- representation. 
class PackedVec v where
  -- | The packed representation of 'v'
  data Packed v      
  pack   :: v -> Packed v
  unpack :: Packed v -> v

{-# RULES 
      "Vec pack/unpack" forall x.
        pack (unpack x) = x; 
      "Vec unpack/pack" forall x.
        unpack (pack x) = x;  #-}




instance PackedVec (Vec2 Int) where 
  data Packed (Vec2 Int) = Vec2I {-#UNPACK#-} !Int {-#UNPACK#-} !Int
  pack (a:.b:.()) = Vec2I a b    
  unpack (Vec2I a b) = a:.b:.() 
  {-# INLINE pack #-}             
  {-# INLINE unpack #-}               

instance PackedVec (Vec3 Int) where 
  data Packed (Vec3 Int) = Vec3I {-#UNPACK#-} !Int {-#UNPACK#-} !Int {-#UNPACK#-} !Int 
  pack (a:.b:.c:.()) = Vec3I a b c; 
  unpack (Vec3I a b c) = a:.b:.c:.();
  {-# INLINE pack #-}                 
  {-# INLINE unpack #-}                 

instance PackedVec (Vec4 Int) where 
  data Packed (Vec4 Int) = Vec4I {-#UNPACK#-} !Int {-#UNPACK#-} !Int  {-#UNPACK#-} !Int {-#UNPACK#-} !Int
  pack (a:.b:.c:.d:.()) = Vec4I a b c d
  unpack (Vec4I a b c d) = a:.b:.c:.d:.()
  {-# INLINE pack #-}
  {-# INLINE unpack #-}


type Vec2I = Packed (Vec2 Int)
type Vec3I = Packed (Vec3 Int)
type Vec4I = Packed (Vec4 Int)




instance PackedVec (Vec2 Float) where 
  data Packed (Vec2 Float) = Vec2F {-#UNPACK#-} !Float {-#UNPACK#-} !Float
  pack (a:.b:.()) = Vec2F a b    
  unpack (Vec2F a b) = a:.b:.() 
  {-# INLINE pack #-}             
  {-# INLINE unpack #-}               

instance PackedVec (Vec3 Float) where 
  data Packed (Vec3 Float) = Vec3F {-#UNPACK#-} !Float {-#UNPACK#-} !Float {-#UNPACK#-} !Float 
  pack (a:.b:.c:.()) = Vec3F a b c; 
  unpack (Vec3F a b c) = a:.b:.c:.();
  {-# INLINE pack #-}                 
  {-# INLINE unpack #-}                 

instance PackedVec (Vec4 Float) where 
  data Packed (Vec4 Float) = Vec4F {-#UNPACK#-} !Float {-#UNPACK#-} !Float  {-#UNPACK#-} !Float {-#UNPACK#-} !Float
  pack (a:.b:.c:.d:.()) = Vec4F a b c d
  unpack (Vec4F a b c d) = a:.b:.c:.d:.()
  {-# INLINE pack #-}
  {-# INLINE unpack #-}

type Vec2F = Packed (Vec2 Float)
type Vec3F = Packed (Vec3 Float)
type Vec4F = Packed (Vec4 Float)





instance PackedVec (Vec2 Double) where 
  data Packed (Vec2 Double) = Vec2D {-#UNPACK#-} !Double {-#UNPACK#-} !Double
  pack (a:.b:.()) = Vec2D a b    
  unpack (Vec2D a b) = a:.b:.() 
  {-# INLINE pack #-}             
  {-# INLINE unpack #-}               

instance PackedVec (Vec3 Double) where 
  data Packed (Vec3 Double) = Vec3D {-#UNPACK#-} !Double {-#UNPACK#-} !Double {-#UNPACK#-} !Double 
  pack (a:.b:.c:.()) = Vec3D a b c; 
  unpack (Vec3D a b c) = a:.b:.c:.();
  {-# INLINE pack #-}                 
  {-# INLINE unpack #-}                 

instance PackedVec (Vec4 Double) where 
  data Packed (Vec4 Double) = Vec4D {-#UNPACK#-} !Double {-#UNPACK#-} !Double  {-#UNPACK#-} !Double {-#UNPACK#-} !Double
  pack (a:.b:.c:.d:.()) = Vec4D a b c d
  unpack (Vec4D a b c d) = a:.b:.c:.d:.()
  {-# INLINE pack #-}
  {-# INLINE unpack #-}

type Vec2D = Packed (Vec2 Double)
type Vec3D = Packed (Vec3 Double)
type Vec4D = Packed (Vec4 Double)

type Mat22D = Vec2 (Vec2D)
type Mat23D = Vec2 (Vec3D)
type Mat24D = Vec2 (Vec4D)
type Mat33D = Vec3 (Vec3D)
type Mat34D = Vec3 (Vec4D)
type Mat44D = Vec4 (Vec4D)


-- | Construct a semi-packed matrix, one whose rows are packed.
packMat ::  (Map a (Packed a) u v, PackedVec a) => u -> v
packMat = map pack

unpackMat ::  (Map (Packed v) v u v1, PackedVec v) => u -> v1
unpackMat = map unpack

instance (Eq v, PackedVec v) => Eq (Packed v) where
  u == v  =  unpack u == unpack v
  u /= v  =  unpack u /= unpack v
  {-# INLINE (==) #-}
  {-# INLINE (/=) #-}

instance (Ord v, PackedVec v) => Ord (Packed v) where
  compare u v = compare (unpack u) (unpack v)
  {-# INLINE compare #-}

instance (Show v, PackedVec v) => Show (Packed v) where
  show v = show (unpack v)

instance (Map a b u v, PackedVec u, PackedVec v) 
          => Map a b (Packed u) (Packed v) 
  where
  map f = pack . map f . unpack
  {-# INLINE map #-}

instance (Fold a v, PackedVec v) => Fold a (Packed v) 
  where
  fold f = fold f . unpack
  foldl f z = foldl f z . unpack
  foldr f z = foldr f z . unpack
  {-# INLINE fold  #-}
  {-# INLINE foldl #-}
  {-# INLINE foldr #-}

instance (ZipWith a b c u v w, PackedVec u, PackedVec v, PackedVec w)
          => ZipWith a b c (Packed u) (Packed v) (Packed w)
  where
  zipWith f u v = pack $ zipWith f (unpack u) (unpack v)
  {-# INLINE zipWith #-}

instance (Num v, PackedVec v) => Num (Packed v) 
  where
  (+) u v = pack (unpack u + unpack v) 
  (-) u v = pack (unpack u - unpack v)
  (*) u v = pack (unpack u * unpack v)
  abs u   = pack (abs (unpack u))
  signum u = pack (signum (unpack u))
  fromInteger i = pack (fromInteger i)
  {-# INLINE (+) #-}
  {-# INLINE (-) #-}
  {-# INLINE (*) #-}
  {-# INLINE abs #-}
  {-# INLINE signum #-}
  {-# INLINE fromInteger #-}
    
instance (Fractional v, PackedVec v) => Fractional (Packed v)
  where
  (/) u v = pack (unpack u / unpack v)
  recip u = pack (recip (unpack u))
  fromRational r = pack (fromRational r)
  {-# INLINE (/) #-}
  {-# INLINE recip #-}
  {-# INLINE fromRational #-}

instance (Storable v, PackedVec v) => Storable (Packed v)
  where
  sizeOf _ = sizeOf (undefined::v)
  alignment _ = alignment (undefined::v)
  peek p = peek (castPtr p) >>= \v -> return (pack v)
  peekByteOff p o = peek (p`plusPtr`o)
  peekElemOff p i = peek (p`plusPtr`(i*sizeOf(undefined::v)))
  poke p v = poke (castPtr p) (unpack v)
  pokeByteOff p o x = poke (p`plusPtr`o) x
  pokeElemOff p i x = poke (p`plusPtr`(i*sizeOf(undefined::v))) x
  {-# INLINE sizeOf #-}
  {-# INLINE alignment #-}
  {-# INLINE peek #-}
  {-# INLINE peekByteOff #-}
  {-# INLINE peekElemOff #-}
  {-# INLINE poke #-}
  {-# INLINE pokeByteOff #-}
  {-# INLINE pokeElemOff #-}


instance (Arbitrary v, PackedVec v) => Arbitrary (Packed v)
  where
  arbitrary = arbitrary >>= return . pack
  coarbitrary v = coarbitrary (unpack v)

  
