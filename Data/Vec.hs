
module Data.Vec 
  (module Data.Vec.Base
  ,module Data.Vec.LinAlg
  ,module Data.Vec.Packed
  ,module Data.Vec.Nat
  )
where

import Data.Vec.Base
import Data.Vec.LinAlg
import Data.Vec.Packed
import Data.Vec.Nat
import Data.Vec.Instances

-- Vec : a library for fixed-length lists, with emphasis on low-dimensional
-- linear algebra.
-- 
-- by Scott E. Dillard, sedillard@gmail.com
--
-- Vectors are lists with type-encoded lengths. The constructor is @:.@, which
-- acts like a cons, both at the value and type levels. @()@ is nil. So
-- @x:.y:.z:.()@ is a 3d vector. @:.@ is strict in both arguments, but it 
-- is also polymorphic, so at runtime vectors will be realized as linked lists,
-- albeit with fewer pattern cases. However the library provides packed
-- representations for 2,3 and 4d vectors of Ints, Floats and Doubles. @Vec3D x
-- y z@ constructs a packed vector of unboxed doubles. Functions @pack@ and
-- @unpack@ convert between packed and unpacked types. Expressions bracketed by
-- @pack@ and @unpack@ will unfold into very efficient code. The @Storable@
-- instances for vectors also generate fast code.
--
-- Matrices are vectors of vectors. Packed matrices are vectors of packed
-- vectors, so Vec2D:.Vec2D:.() is a 2x2 of doubles. 
--
-- This library makes heavy use of functional dependencies. I have tried to
-- tweak things so that they "just work." However, every now and then you will
-- get incromprehensible error messages, usually about how this isn't an
-- instance of that. These are how type errors typically manifest, so first
-- double check to make sure you aren't trying to mix vectors of different
-- dimension or component types. If you still get these errors, manual type
-- annotations usually make them go away.
--
-- see previous independent work by David Menendez : 
--  http://haskell.org/pipermail/haskell/2005-May/015815.html
