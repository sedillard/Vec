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

import Data.Vec.Base as V
import Data.Vec.Nat
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


--Num and Fractional instances. Everything is done component-wise. This is not
--at all consistent with mathematical convention, but I find it convenient. For
--instance, fromIntegral and realToFrac create uniform vectors from their
--arguments, so multiplying a vector by a scalar is just 2 * v, and likewise
--for multiplying a matrix by a scalar. The literal 0 gives you either the null
--vector or a matrix of zeros, depending on the type. Anyway, if dot=sum(v*v) offends
--you, then just write your own instances and ignore these.

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
