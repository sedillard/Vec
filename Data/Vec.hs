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

{- |

Vec : a library for fixed-length lists and low-dimensional linear algebra

Scott E. Dillard <sedillard@gmail.com>

darcs : <http://graphics.cs.ucdavis.edu/~sdillard/Vec>

/Synopsis/

Vectors are represented by lists with type-encoded lengths. The constructor is
@:.@, which acts like a cons both at the value and type levels, with @()@
taking the place of nil. So @x:.y:.z:.()@ is a 3d vector. The library provides
a set of common list-like functions (map, fold, etc) for working with vectors.
Built up from these functions are a small but useful set of linear algebra
operations: matrix multiplication, determinants, solving linear systems,
inverting matrices.

/Design/


* Simplicity : 
Beyond the initial complexities of type-level lists and
numbers, I've tried to keep the API simple. There is no vector-space
class, nor a complicated hierarchy of linear\/affine\/projective
transformations. These can be added on top of the library easily.

* Purity :
The library is written in the functional style. For most
functions this does not hinder performance at all, but some I am still
working on (Gaussian elimination) so if this library is a bottleneck you
can easily drop down to C. 

* Low Dimension :
Although the dimensionality is limited only by what GHC
will handle, the library is meant for 2,3 and 4 dimensions. For general
linear algebra, check out the excellent hmatrix library and blas bindings.

To the point of simplicity, vectors and matrices are instances of Num and
Fractional.  All arithmetic is done component-wise and literals construct
uniform vectors and matrices. There are many interesting projects aiming to
overhaul Haskell's number classes, but for now the type of @(*)@ is @a -> a ->
a@ so that's what we're working with. It is easy to incorporate this library
into a more mathematically consistent class hierarchy (provided you can design
one.) 

The rule is simple : 
  If the method is unary, it's a map. 
  If it's binary, it's a zipWith.

/Performance/

@(:.)@ is strict in both arguments, but it is also polymorphic, so at runtime
vectors will be realized as linked lists, albeit with less pattern matching.
However the library provides packed representations for 2,3 and 4d vectors of
Ints, Floats and Doubles. @'Vec3F' x y z@ constructs a packed vector of
unboxed Floats. Functions @'pack'@ and @'unpack'@ convert between packed and
unpacked types. When vector operations are bracketed by 'pack' and 'unpack',
GHC can unfold them into very efficient code. The 'Storable' instances for
vectors also generate fast code.  Without optimizations, the code falls back
into linked-list mode. The optimizations depend on inlining, so you may need
to increase your unfolding threshold in certain situations.

/GHC Extensions/

This library makes heavy use of functional dependencies. I have tried to
tweak things so that they \"just work.\" However, every now and then you will
get incomprehensible error messages, usually about how this isn't an
instance of that. These are how type errors typically manifest, so first
double check to make sure you aren't trying to mix vectors of different
dimension or component types. If you still get these errors, manual type
annotations usually make them go away.


/Related Work/

See previous work by David Menendez,
  <http://haskell.org/pipermail/haskell/2005-May/015815.html>

and of course Oleg Kiselyov,
  <http://okmij.org/ftp/papers/number-parameterized-types.pdf>

Other vector and linear algebra packages :

vector-space, by Conal Elliott : 
  <http://hackage.haskell.org/cgi-bin/hackage-scripts/package/vector-space>

hmatrix, by Alberto Ruiz :
  <http://hackage.haskell.org/cgi-bin/hackage-scripts/package/hmatrix>

blas bindings, by Patrick Perry :
  <http://hackage.haskell.org/cgi-bin/hackage-scripts/package/blas>

templatized geometry library (C++), by Oliver Kreylos :
  <http://graphics.cs.ucdavis.edu/~okreylos/ResDev/Geometry/index.html>
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


