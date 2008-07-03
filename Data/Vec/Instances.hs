{- Copyright (c) 2008, Scott E. Dillard. All rights reserved. -}
{-# OPTIONS -cpp #-}

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

module Data.Vec.Instances where

import Prelude hiding (map,foldl,foldr,zipWith)
import Data.Vec.Base as V
import Data.Vec.Nat
import Data.Vec.Packed
import Foreign.Storable
import Foreign.Ptr

-- Storable instances. 

instance Storable a => Storable (a:.()) where
  sizeOf _ = sizeOf (undefined::a)
  alignment _ = alignment (undefined::a)
  peek !p = peek (castPtr p) >>= \a -> return (a:.())
  peekByteOff !p !o = peek (p`plusPtr`o)
  peekElemOff !p !i = peek (p`plusPtr`(i*sizeOf(undefined::a)))
  poke !p (a:._) = poke (castPtr p) a
  pokeByteOff !p !o !x = poke (p`plusPtr`o) x
  pokeElemOff !p !i !x = poke (p`plusPtr`(i*sizeOf(undefined::a))) x
  {-# INLINE sizeOf #-}
  {-# INLINE alignment #-}
  {-# INLINE peek #-}
  {-# INLINE peekByteOff #-}
  {-# INLINE peekElemOff #-}
  {-# INLINE poke #-}
  {-# INLINE pokeByteOff #-}
  {-# INLINE pokeElemOff #-}

instance (Vec (Succ (Succ n)) a (a:.a:.v), Storable a, Storable (a:.v)) 
  => Storable (a:.a:.v) 
  where
  sizeOf _ = sizeOf (undefined::a) + sizeOf (undefined::(a:.v))
  alignment _ = alignment (undefined::a)
  peek !p = 
    peek (castPtr p) >>= \a -> 
    peek (castPtr (p`plusPtr`sizeOf(undefined::a))) >>= \v -> 
    return (a:.v)
  peekByteOff !p !o = peek (p`plusPtr`o)
  peekElemOff !p !i = peek (p`plusPtr`(i*sizeOf(undefined::(a:.a:.v))))
  poke !p (a:.v) = 
    poke (castPtr p) a >> 
    poke (castPtr (p`plusPtr`sizeOf(undefined::a))) v
  pokeByteOff !p !o !x = poke (p`plusPtr`o) x
  pokeElemOff !p !i !x = poke (p`plusPtr`(i*sizeOf(undefined::(a:.a:.v)))) x
  {-# INLINE sizeOf #-}
  {-# INLINE alignment #-}
  {-# INLINE peek #-}
  {-# INLINE peekByteOff #-}
  {-# INLINE peekElemOff #-}
  {-# INLINE poke #-}
  {-# INLINE pokeByteOff #-}
  {-# INLINE pokeElemOff #-}


-- Num and Fractional instances : All arithmetic is done component-wise and
-- literals construct uniform vectors and matrices. 
--
-- The rule is simple : 
--    If the method is unary, it's a map.  
--    If it's binary, it's a zipWith.
--
-- You are free to ignore these instances if the definition of (*) offends you.

instance
    (Eq (a:.u)
    ,Show (a:.u)
    ,Num a
    ,Map a a (a:.u) (a:.u) 
    ,ZipWith a a a (a:.u) (a:.u) (a:.u)
    ,Vec (Succ l) a (a:.u)
    )
    => Num (a:.u) 
  where
    (+) u v = V.zipWith (+) u v 
    (-) u v = V.zipWith (-) u v
    (*) u v = V.zipWith (*) u v
    abs u = V.map abs u
    signum u = V.map signum u
    fromInteger i = vec (fromInteger i)
    {-# INLINE (+) #-}
    {-# INLINE (-) #-}
    {-# INLINE (*) #-}
    {-# INLINE abs #-}
    {-# INLINE signum #-}
    {-# INLINE fromInteger #-}


instance 
    (Fractional a
    ,Ord (a:.u)
    ,ZipWith a a a (a:.u) (a:.u) (a:.u)
    ,Map a a (a:.u) (a:.u)
    ,Vec (Succ l) a (a:.u)
    ,Show (a:.u)
    ) 
    => Fractional (a:.u) 
  where
    (/) u v = V.zipWith (/) u v
    recip u = V.map recip u
    fromRational r = vec (fromRational r)
    {-# INLINE (/) #-}
    {-# INLINE recip #-}
    {-# INLINE fromRational #-}

#if 1


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

#endif
