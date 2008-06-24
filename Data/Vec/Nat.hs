{- Copyright (c) 2008, Scott E. Dillard. All rights reserved. -}

{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Type level naturals. @Ni@ is a type, @ni@ an undefined value of that type,
-- for @i <- [0..19]@
module Data.Vec.Nat where


data N0
data Succ a

type N1  = Succ N0
type N2  = Succ N1
type N3  = Succ N2
type N4  = Succ N3
type N5  = Succ N4
type N6  = Succ N5
type N7  = Succ N6
type N8  = Succ N7
type N9  = Succ N8
type N10 = Succ N9
type N11 = Succ N10
type N12 = Succ N11
type N13 = Succ N12
type N14 = Succ N13
type N15 = Succ N14
type N16 = Succ N15
type N17 = Succ N16
type N18 = Succ N17
type N19 = Succ N18

n0  :: N0  ; n0  = undefined
n1  :: N1  ; n1  = undefined
n2  :: N2  ; n2  = undefined
n3  :: N3  ; n3  = undefined
n4  :: N4  ; n4  = undefined
n5  :: N5  ; n5  = undefined
n6  :: N6  ; n6  = undefined
n7  :: N7  ; n7  = undefined
n8  :: N8  ; n8  = undefined
n9  :: N9  ; n9  = undefined
n10 :: N10 ; n10  = undefined
n11 :: N11 ; n11  = undefined
n12 :: N12 ; n12  = undefined
n13 :: N13 ; n13  = undefined
n14 :: N14 ; n14  = undefined
n15 :: N15 ; n15  = undefined
n16 :: N16 ; n16  = undefined
n17 :: N17 ; n17  = undefined
n18 :: N18 ; n18  = undefined
n19 :: N19 ; n19  = undefined

-- | @nat n@ yields the @Int@ value of the type-level natural @n@.
class Nat n where nat :: n -> Int
instance Nat N0 where nat _ = 0
instance Nat a => Nat (Succ a) where nat _ = 1+(nat (undefined::a))

class Pred x y | x -> y, y -> x
instance Pred (Succ N0) N0
instance Pred (Succ n) p => Pred (Succ (Succ n)) (Succ p)

