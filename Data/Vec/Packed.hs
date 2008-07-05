{- Copyright (c) 2008, Scott E. Dillard. All rights reserved. -}
{-# OPTIONS -cpp #-}


{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Packed vectors : use these whenever possible. The regular vector type is
-- just a gussied up linked list, but when vector functions are applied to
-- these types, bracketed by @'pack'@ and @'unpack'@, then things unfold into
-- neatly optimized code. 
-- 
-- Storable, Num, Fractional, Fold, Map, and ZipWith instances are provided for
-- packed vectors, so some operations do not require pack/unpack. For example,
-- @'dot'@ does not require pack/unpack because it is defined in terms of
-- @'zipWith'@ and @'fold'@. However @'transpose'@, @'det'@, @'gaussElim'@ and
-- most others are recursive, and so you'll still need to use pack/unpack with
-- these. This goes for @'multmm'@ as well because it uses @'transpose'@, and
-- @'multmv'@ does not need its arguments to be unpacked, but the result will
-- be a polymorphic vector (:.) so you will want to pack it again. This is all
-- very experimental and likely to change.

module Data.Vec.Packed where

import Prelude hiding (map,foldl,foldr,zipWith)
import Data.Vec.Base as V
import Data.Vec.Instances
import Foreign

-- | PackedVec class : relates a packed vector type to its unpacked type For
-- now, the fundep is not bijective -- It may be advantageous to have multiple
-- packed representations for a canonical vector type. This may change. In the
-- meantime, you may have to annotate return types.
class PackedVec pv v | pv -> v  where
  pack   :: v -> pv
  unpack :: pv -> v

{-# RULES 
      "Vec pack/unpack" forall x.
        pack (unpack x) = x;
      "Vec unpack/pack" forall x.
        unpack (pack x) = x;
      "Vec pack.unpack" 
        pack . unpack = id;
      "Vec unpack.pack"
        unpack . pack = id; #-}

-- * Packed Vector Types
data Vec2I = Vec2I {-#UNPACK#-} !Int 
                   {-#UNPACK#-} !Int 
                   deriving (Eq,Ord,Show,Read)

data Vec3I = Vec3I {-#UNPACK#-} !Int 
                   {-#UNPACK#-} !Int 
                   {-#UNPACK#-} !Int
                   deriving (Eq,Ord,Show,Read)

data Vec4I = Vec4I {-#UNPACK#-} !Int 
                   {-#UNPACK#-} !Int 
                   {-#UNPACK#-} !Int
                   {-#UNPACK#-} !Int
                   deriving (Eq,Ord,Show,Read)

data Vec2F = Vec2F {-#UNPACK#-} !Float 
                   {-#UNPACK#-} !Float 
                   deriving (Eq,Ord,Show,Read)

data Vec3F = Vec3F {-#UNPACK#-} !Float 
                   {-#UNPACK#-} !Float 
                   {-#UNPACK#-} !Float
                   deriving (Eq,Ord,Show,Read)

data Vec4F = Vec4F {-#UNPACK#-} !Float 
                   {-#UNPACK#-} !Float 
                   {-#UNPACK#-} !Float
                   {-#UNPACK#-} !Float
                   deriving (Eq,Ord,Show,Read)

data Vec2D = Vec2D {-#UNPACK#-} !Double 
                   {-#UNPACK#-} !Double 
                   deriving (Eq,Ord,Show,Read)

data Vec3D = Vec3D {-#UNPACK#-} !Double 
                   {-#UNPACK#-} !Double 
                   {-#UNPACK#-} !Double
                   deriving (Eq,Ord,Show,Read)

data Vec4D = Vec4D {-#UNPACK#-} !Double 
                   {-#UNPACK#-} !Double 
                   {-#UNPACK#-} !Double
                   {-#UNPACK#-} !Double
                   deriving (Eq,Ord,Show,Read)

-- * Packed Matrix Types. 
type Mat22I = Vec2 Vec2I 
type Mat23I = Vec2 Vec3I 
type Mat33I = Vec3 Vec3I 
type Mat34I = Vec3 Vec4I 
type Mat44I = Vec4 Vec3I 

type Mat22F = Vec2 Vec2F 
type Mat23F = Vec2 Vec3F 
type Mat33F = Vec3 Vec3F 
type Mat34F = Vec3 Vec4F 
type Mat44F = Vec4 Vec3F 

type Mat22D = Vec2 Vec2D 
type Mat23D = Vec2 Vec3D 
type Mat33D = Vec3 Vec3D 
type Mat34D = Vec3 Vec4D 
type Mat44D = Vec4 Vec4D 


-- | pack a matrix
packMat ::  (Map v pv m pm, PackedVec pv v) => m -> pm
packMat = V.map pack 
{-# INLINE packMat #-}

-- | unpack a matrix
unpackMat ::  (Map pv v pm m, PackedVec pv v) => pm -> m
unpackMat = V.map unpack
{-# INLINE unpackMat #-}


instance PackedVec Vec2I (Vec2 Int) where
  pack (x:.y:.()) = Vec2I x y 
  unpack (Vec2I x y) = x:.y:.()
  {-# INLINE pack #-}
  {-# INLINE unpack #-}

instance PackedVec Vec3I (Vec3 Int) where
  pack (x:.y:.z:.()) = Vec3I x y z
  unpack (Vec3I x y z) = x:.y:.z:.()
  {-# INLINE pack #-}
  {-# INLINE unpack #-}

instance PackedVec Vec4I (Vec4 Int) where
  pack (x:.y:.z:.w:.()) = Vec4I x y z w
  unpack (Vec4I x y z w) = x:.y:.z:.w:.()
  {-# INLINE pack #-}
  {-# INLINE unpack #-}


instance PackedVec Vec2F (Vec2 Float) where
  pack (x:.y:.()) = Vec2F x y 
  unpack (Vec2F x y) = x:.y:.()
  {-# INLINE pack #-}
  {-# INLINE unpack #-}

instance PackedVec Vec3F (Vec3 Float) where
  pack (x:.y:.z:.()) = Vec3F x y z
  unpack (Vec3F x y z) = x:.y:.z:.()
  {-# INLINE pack #-}
  {-# INLINE unpack #-}

instance PackedVec Vec4F (Vec4 Float) where
  pack (x:.y:.z:.w:.()) = Vec4F x y z w
  unpack (Vec4F x y z w) = x:.y:.z:.w:.()
  {-# INLINE pack #-}
  {-# INLINE unpack #-}


instance PackedVec Vec2D (Vec2 Double) where
  pack (x:.y:.()) = Vec2D x y 
  unpack (Vec2D x y) = x:.y:.()
  {-# INLINE pack #-}
  {-# INLINE unpack #-}

instance PackedVec Vec3D (Vec3 Double) where
  pack (x:.y:.z:.()) = Vec3D x y z
  unpack (Vec3D x y z) = x:.y:.z:.()
  {-# INLINE pack #-}
  {-# INLINE unpack #-}

instance PackedVec Vec4D (Vec4 Double) where
  pack (x:.y:.z:.w:.()) = Vec4D x y z w
  unpack (Vec4D x y z w) = x:.y:.z:.w:.()
  {-# INLINE pack #-}
  {-# INLINE unpack #-}

#define MAP_INSTANCE(S,V) \
instance Map S S V V where map f = pack . map f . unpack

MAP_INSTANCE(Int,Vec2I)
MAP_INSTANCE(Int,Vec3I)
MAP_INSTANCE(Int,Vec4I)
MAP_INSTANCE(Float,Vec2F)
MAP_INSTANCE(Float,Vec3F)
MAP_INSTANCE(Float,Vec4F)
MAP_INSTANCE(Double,Vec2D)
MAP_INSTANCE(Double,Vec3D)
MAP_INSTANCE(Double,Vec4D)


#define FOLD_INSTANCE(S,V)        \
instance Fold S V where           \
  fold  f   = fold  f   . unpack; \
  foldl f z = foldl f z . unpack; \
  foldr f z = foldr f z . unpack;

FOLD_INSTANCE(Int,Vec2I)
FOLD_INSTANCE(Int,Vec3I)
FOLD_INSTANCE(Int,Vec4I)
FOLD_INSTANCE(Float,Vec2F)
FOLD_INSTANCE(Float,Vec3F)
FOLD_INSTANCE(Float,Vec4F)
FOLD_INSTANCE(Double,Vec2D)
FOLD_INSTANCE(Double,Vec3D)
FOLD_INSTANCE(Double,Vec4D)

#define ZIPWITH_INSTANCE(S,V) \
instance ZipWith S S S V V V where \
  zipWith f u v = pack $ zipWith f (unpack u) (unpack v)

ZIPWITH_INSTANCE(Int,Vec2I)
ZIPWITH_INSTANCE(Int,Vec3I)
ZIPWITH_INSTANCE(Int,Vec4I)
ZIPWITH_INSTANCE(Float,Vec2F)
ZIPWITH_INSTANCE(Float,Vec3F)
ZIPWITH_INSTANCE(Float,Vec4F)
ZIPWITH_INSTANCE(Double,Vec2D)
ZIPWITH_INSTANCE(Double,Vec3D)
ZIPWITH_INSTANCE(Double,Vec4D)


#define NUM_INSTANCE(V)                 \
instance Num V where                    \
  u + v  = pack $ unpack u + unpack v ; \
  u - v  = pack $ unpack u + unpack v ; \
  u * v  = pack $ unpack u * unpack v ; \
  abs    = pack . abs . unpack    ;     \
  signum = pack . signum . unpack ;     \
  fromInteger = pack . fromInteger

NUM_INSTANCE(Vec2I)
NUM_INSTANCE(Vec3I)
NUM_INSTANCE(Vec4I)
NUM_INSTANCE(Vec2F)
NUM_INSTANCE(Vec3F)
NUM_INSTANCE(Vec4F)
NUM_INSTANCE(Vec2D)
NUM_INSTANCE(Vec3D)
NUM_INSTANCE(Vec4D)

#define FRACTIONAL_INSTANCE(V)        \
instance Fractional V where           \
  u / v = pack $ unpack u / unpack v ;\
  recip = pack . recip . unpack      ;\
  fromRational = pack . fromRational

FRACTIONAL_INSTANCE(Vec2F)
FRACTIONAL_INSTANCE(Vec3F)
FRACTIONAL_INSTANCE(Vec4F)
FRACTIONAL_INSTANCE(Vec2D)
FRACTIONAL_INSTANCE(Vec3D)
FRACTIONAL_INSTANCE(Vec4D)


#define STORABLE_INSTANCE(V,PV)                                  \
instance Storable PV where                                       \
  sizeOf = sizeOf.unpack                                        ;\
  alignment = alignment.unpack                                  ;\
  peek p = peek (castPtr p) >>= return.pack                     ;\
  poke p v = poke (castPtr p) (unpack v)                        ;\
  peekElemOff p i   = peekElemOff (castPtr p) i >>= return.pack ;\
  pokeElemOff p i v = pokeElemOff (castPtr p) i (unpack v)      ;\
  peekByteOff p b   = peekByteOff (castPtr p) b >>= return.pack ;\
  pokeByteOff p b v = pokeByteOff (castPtr p) b (unpack v)      ;\

STORABLE_INSTANCE(Vec2 Int,Vec2I)
STORABLE_INSTANCE(Vec3 Int,Vec3I)
STORABLE_INSTANCE(Vec4 Int,Vec4I)
STORABLE_INSTANCE(Vec2 Float,Vec2F)
STORABLE_INSTANCE(Vec3 Float,Vec3F)
STORABLE_INSTANCE(Vec4 Float,Vec4F)
STORABLE_INSTANCE(Vec2 Double,Vec2D)
STORABLE_INSTANCE(Vec3 Double,Vec3D)
STORABLE_INSTANCE(Vec4 Double,Vec4D)

