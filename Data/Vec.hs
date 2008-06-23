{-
Copyright (c) 2008, Scott E. Dillard
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice,
      this list of conditions and the following disclaimer in the documentation
      and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

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
