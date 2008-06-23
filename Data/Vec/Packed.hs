{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Vec.Packed where

import Data.Vec.Base as V

-- packed vectors : use these whenever possible. The regular vector type is
-- just a gussied up linked list, but when vector functions are applied to
-- these types, bracketed by pack and unpack, then things unfold into
-- perfectly optimized code.

data Vec2I = Vec2I {-#UNPACK#-} !Int 
                   {-#UNPACK#-} !Int 

data Vec3I = Vec3I {-#UNPACK#-} !Int 
                   {-#UNPACK#-} !Int 
                   {-#UNPACK#-} !Int

data Vec4I = Vec4I {-#UNPACK#-} !Int 
                   {-#UNPACK#-} !Int 
                   {-#UNPACK#-} !Int
                   {-#UNPACK#-} !Int

data Vec2F = Vec2F {-#UNPACK#-} !Float 
                   {-#UNPACK#-} !Float 

data Vec3F = Vec3F {-#UNPACK#-} !Float 
                   {-#UNPACK#-} !Float 
                   {-#UNPACK#-} !Float

data Vec4F = Vec4F {-#UNPACK#-} !Float 
                   {-#UNPACK#-} !Float 
                   {-#UNPACK#-} !Float
                   {-#UNPACK#-} !Float

data Vec2D = Vec2D {-#UNPACK#-} !Double 
                   {-#UNPACK#-} !Double 

data Vec3D = Vec3D {-#UNPACK#-} !Double 
                   {-#UNPACK#-} !Double 
                   {-#UNPACK#-} !Double

data Vec4D = Vec4D {-#UNPACK#-} !Double 
                   {-#UNPACK#-} !Double 
                   {-#UNPACK#-} !Double
                   {-#UNPACK#-} !Double

-- (Semi) packed matrices. The rows are packed.
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


-- pack and unpack a matrix
packMat = V.map pack 
unpackMat = V.map unpack
{-# INLINE packMat #-}
{-# INLINE unpackMat #-}

-- PackedVec class : relates a packed vector type to its unpacked type
-- For now, the fundep is not bijective -- It may be advantageous to have
-- multiple packed representations for a canonical vector type. This may change.
class PackedVec pv v | pv -> v  where
  pack   :: v -> pv
  unpack :: pv -> v

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



