
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
-- see previous (independent) work by David Menendez : 
--  http://haskell.org/pipermail/haskell/2005-May/015815.html
