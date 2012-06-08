{- Copyright (c) 2008, Scott E. Dillard. All rights reserved. -}

{-# LANGUAGE EmptyDataDecls            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UnboxedTuples             #-}
{-# LANGUAGE UndecidableInstances      #-}

-- {-# HADDOCK_OPTIONS prune #-}

module Data.Vec.Base where

import Data.Vec.Nat

import Prelude hiding (map,zipWith,foldl,foldr,reverse,
                       take,drop,head,tail,sum,last,product,
                       minimum,maximum,length)
import qualified Prelude as P
import Foreign

--for UArray instances
import Data.Array.Base  as Array
import GHC.ST		( ST(..), runST )
import GHC.Prim     
import GHC.Base         ( Int(..) )
import GHC.Float	( Float(..), Double(..) )
import GHC.Word		( Word8(..) )


-- | The vector constructor. @(:.)@ for vectors is like @(:)@ for lists, and
-- @()@ takes the place of @[]@. (The list of instances here is not meant to be
-- readable.)

data a :. b = !a :. !b
  deriving (Eq,Ord,Read)

infixr :.

--derived show outputs in prefix notation
instance (Show a, ShowVec v) => Show (a:.v) where
  show (a:.v) = "(" ++ show a ++ "):." ++ showVec v


-- Helper to keep parentheses at bay. Just use @show@ as usual.
class ShowVec  v where
  showVec :: v -> String

instance ShowVec () where
  showVec = show
  {-# INLINE showVec #-}

instance (Show a, ShowVec v) => ShowVec (a:.v) where
  showVec (a:.v) = "(" ++ show a ++ "):." ++ showVec v
  {-# INLINE showVec #-}


-- * Vector Types
type Vec2  a = a :. a :. ()
type Vec3  a = a :. (Vec2 a)
type Vec4  a = a :. (Vec3 a)
type Vec5  a = a :. (Vec4 a)
type Vec6  a = a :. (Vec5 a)
type Vec7  a = a :. (Vec6 a)
type Vec8  a = a :. (Vec7 a)
type Vec9  a = a :. (Vec8 a)
type Vec10 a = a :. (Vec9 a)
type Vec11 a = a :. (Vec10 a)
type Vec12 a = a :. (Vec11 a)
type Vec13 a = a :. (Vec12 a)
type Vec14 a = a :. (Vec13 a)
type Vec15 a = a :. (Vec14 a)
type Vec16 a = a :. (Vec15 a)
type Vec17 a = a :. (Vec16 a)
type Vec18 a = a :. (Vec17 a)
type Vec19 a = a :. (Vec18 a)




-- | The type constraint @Vec n a v@ infers the vector type @v@ from the
-- length @n@, a type-level natural, and underlying component type @a@.  
-- So @x :: Vec N4 a v => v@ declares @x@ to be a 4-vector of @a@s.

class Vec n a v | n a -> v, v -> n a where
  -- | Make a uniform vector of a given length. @n@ is a type-level natural.
  -- Use `vec` when the length can be inferred.
  mkVec :: n -> a -> v


instance Vec N1 a ( a :. () ) where
  mkVec _ a = a :. ()
  {-# INLINE mkVec #-}

instance Vec (Succ n) a (a':.v) => Vec (Succ (Succ n)) a (a:.a':.v) where
  mkVec _ a = a :. (mkVec undefined a)
  {-# INLINE mkVec #-}


-- | Make a uniform vector. The length is inferred.
vec ::  (Vec n a v) => a -> v
vec = mkVec undefined
{-# INLINE vec #-}


-- | Build a vector from a list, or access vector elements using run-time
-- indicies, numbered from 0.

class VecList a v | v -> a where
  -- | Turn a list into a vector of inferred length. The list must be at least
  -- as long as the vector, but may be longer. Make a mental note of the
  -- distinction between this and 'matFromList', as you might accidentally use
  -- this when you mean that. Because number literals can be converted to
  -- vectors, and matrices are vectors of vectors, the following works
  -- 
  -- > fromList [1,2,3,4] :: Mat22 Int 
  -- > > ((1):.(1):.()):.((2):.(2):.()):.()
  --
  -- even though we meant to do this
  --
  -- > matFromList [1,2,3,4] :: Mat22 Int 
  -- > > ((1):.(2):.()):.((3):.(4):.()):.()
  fromList :: [a] -> v

  -- | Get a vector element, which one determined at runtime.
  getElem :: Int -> v -> a

  -- | Set a vector element, which one determined at runtime
  setElem :: Int -> a -> v -> v

instance VecList a (a:.()) where
  fromList (a:_)   = a :. ()
  fromList []      = error "fromList: list too short"
  getElem i (a :. _)
    | i == 0    = a
    | otherwise = error "getElem: index out of bounds"
  setElem i a _ 
    | i == 0    = a :. ()
    | otherwise = error "setElem: index out of bounds"
  {-# INLINE setElem #-}
  {-# INLINE getElem #-}
  {-# INLINE fromList #-}

instance VecList a (a':.v) => VecList a (a:.(a':.v)) where
  fromList (a:as)  = a :. fromList as
  fromList []      = error "fromList: list too short"
  getElem i (a :. v)
    | i == 0    = a
    | otherwise = getElem (i-1) v
  setElem i a' (a :. v)
    | i == 0    = a' :. v
    | otherwise = a :. (setElem (i-1) a' v)
  {-# INLINE setElem #-}
  {-# INLINE getElem #-}
  {-# INLINE fromList #-}

-- | get or set a vector element, known at compile
--time. Use the Nat types to access vector components. For instance, @get n0@
--gets the x component, @set n2 44@ sets the z component to 44. 

class Access n a v | v -> a where
  get  :: n -> v -> a
  set  :: n -> a -> v -> v

instance Access N0 a (a :. v) where
  get _ (a :. _) = a
  set _ a (_ :. v) = a :. v
  {-# INLINE set #-}
  {-# INLINE get #-}

instance Access n a v => Access (Succ n) a (a :. v) where
  get _ (_ :. v) = get (undefined::n) v
  set _ a' (a :. v) = a :. (set (undefined::n) a' v)
  {-# INLINE set #-}
  {-# INLINE get #-}


-- * List-like functions

-- | The first element.

class Head v a | v -> a  where 
  head :: v -> a

instance Head (a :. as) a where 
  head (a :. _) = a
  {-# INLINE head #-}


-- | All but the first element. 

class Tail v v_ | v -> v_ where 
  tail :: v -> v_

instance Tail (a :. as) as where 
  tail (_ :. as) = as
  {-# INLINE tail #-}




-- | Apply a function over each element in a vector. Constraint @Map a b u v@
-- states that @u@ is a vector of @a@s, @v@ is a vector of @b@s with the same
-- length as @u@, and the function is of type @a -> b@.

class Map a b u v | u -> a, v -> b, b u -> v, a v -> u where
  map :: (a -> b) -> u -> v

instance Map a b (a :. ()) (b :. ()) where
  map f (x :. ()) = (f x) :. ()
  {-# INLINE map #-}

instance Map a b (a':.u) (b':.v) => Map a b (a:.a':.u) (b:.b':.v) where
  map f (x:.v) = (f x):.(map f v)
  {-# INLINE map #-}




-- | Combine two vectors using a binary function. The length of the result is
-- the min of the lengths of the arguments. The constraint @ZipWith a b c u v
-- w@ states that @u@ is a vector of @a@s, @v@ is a vector of @b@s, @w@ is a
-- vector of @c@s, and the binary function is of type @a -> b -> c@.

class ZipWith a b c u v w | u->a, v->b, w->c, u v c -> w where
  zipWith :: (a -> b -> c) -> u -> v -> w

instance ZipWith a b c (a:.()) (b:.()) (c:.()) where
  zipWith f (x:._) (y:._) = f x y :.()
  {-# INLINE zipWith #-}

instance ZipWith a b c (a:.()) (b:.b:.bs) (c:.()) where
  zipWith f (x:._) (y:._) = f x y :.()
  {-# INLINE zipWith #-}

instance ZipWith a b c (a:.a:.as) (b:.()) (c:.()) where
  zipWith f (x:._) (y:._) = f x y :.()
  {-# INLINE zipWith #-}

instance 
  ZipWith a b c (a':.u) (b':.v) (c':.w) 
  => ZipWith a b c (a:.a':.u) (b:.b':.v) (c:.c':.w) 
    where
      zipWith f (x:.u) (y:.v) = f x y :. zipWith f u v
      {-# INLINE zipWith #-}


-- | Fold a function over a vector. 

class Fold v a | v -> a where
  fold  :: (a -> a -> a) -> v -> a
  foldl :: (b -> a -> b) -> b -> v -> b
  foldr :: (a -> b -> b) -> b -> v -> b

instance Fold (a:.()) a where
  fold  _   (a:._) = a 
  foldl f z (a:._) = seq z $ f z a
  foldr f z (a:._) = f a z
  {-# INLINE fold #-}
  {-# INLINE foldl #-}
  {-# INLINE foldr #-}

instance Fold (a':.u) a => Fold (a:.a':.u) a where
  fold  f   (a:.v) = f a (fold f v)
  foldl f z (a:.v) = seq z $ f (foldl f z v) a
  foldr f z (a:.v) = f a (foldr f z v)
  {-# INLINE fold #-}
  {-# INLINE foldl #-}
  {-# INLINE foldr #-}

-- | Reverse a vector 
reverse ::  (Reverse' () v v') => v -> v'
reverse v = reverse' () v
{-# INLINE reverse #-}

-- really the type of reverse should b v->v but somehow this makes inference easier


-- | Reverse helper function : accumulates the reversed list in its first argument
class Reverse' p v v' | p v -> v' where
  reverse' :: p -> v -> v'
  
instance Reverse' p () p where
  reverse' p () = p
  {-# INLINE reverse' #-}

instance Reverse' (a:.p) v v' => Reverse' p (a:.v) v' where
  reverse' p (a:.v) = reverse' (a:.p) v 
  {-# INLINE reverse' #-}


-- | Append two vectors 

class Append v1 v2 v3 | v1 v2 -> v3, v1 v3 -> v2 where 
  append :: v1 -> v2 -> v3

instance Append () v v where
  append _ = id
  {-# INLINE append #-}

instance Append (a:.()) v (a:.v) where
  append (a:.()) v = a:.v
  {-# INLINE append #-}

instance (Append (a':.v1) v2 v3) => Append (a:.a':.v1) v2 (a:.v3) where
  append (a:.u) v  =  a:.(append u v)
  {-# INLINE append #-}



-- | @take n v@ constructs a vector from the first @n@ elements of @v@. @n@ is a
-- type-level natural. For example @take n3 v@ makes a 3-vector of the first
-- three elements of @v@.

class Take n v v' | n v -> v' where
  take :: n -> v -> v'

instance Take N0 v () where
  take _ _ = ()
  {-# INLINE take #-}

instance Take n v v' 
         => Take (Succ n) (a:.v) (a:.v') where
  take _ (a:.v) = a:.(take (undefined::n) v)
  {-# INLINE take #-}


-- | @drop n v@ strips the first @n@ elements from @v@. @n@ is a type-level
-- natural. For example @drop n2 v@ drops the first two elements.

class Drop n v v' | n v -> v' where
  drop :: n -> v -> v'
 
instance Drop N0 v v where
  drop _ = id
  {-# INLINE drop #-}

instance (Drop n (a:.v) v') 
          => Drop (Succ n) (a:.a:.v) v' where
  drop _ (_:.v) = drop (undefined::n) v
  {-# INLINE drop #-}


-- | Get the last element, usually significant for some reason (quaternions,
-- homogenous coordinates, whatever)
class Last v a | v -> a where
  last :: v -> a

instance Last (a:.()) a where 
  last (a:._) = a
  {-# INLINE last #-}

instance Last (a':.v) a => Last (a:.a':.v) a where
  last (_:.v) = last v
  {-# INLINE last #-}


-- | @snoc v a@ appends the element a to the end of v. 
class Snoc v a v' | v a -> v', v' -> v a where 
  snoc :: v -> a -> v'

instance Snoc () a (a:.()) where
  snoc _ a = (a:.())
  {-# INLINE snoc #-}

instance Snoc v a (a:.v) => Snoc (a:.v) a (a:.a:.v) where
  snoc (b:.v) a = b:.(snoc v a)
  {-# INLINE snoc #-}


-- | The length of a vector
class Length v n | v -> n where
  length :: v -> Int

instance Length () N0 where
  length _ = 0

instance (Length v n) => Length (a:.v) (Succ n) where
  length _ = 1+length (undefined::v)


-- | sum of vector elements
sum ::  (Fold v a, Num a) => v -> a
sum x     = fold (+) x
{-# INLINE sum #-}

-- | product of vector elements
product ::  (Fold v a, Num a) => v -> a
product x = fold (*) x
{-# INLINE product #-}

-- | maximum vector element
maximum ::  (Fold v a, Ord a) => v -> a
maximum x = fold max x
{-# INLINE maximum #-}

-- | minimum vector element
minimum ::  (Fold v a, Ord a) => v -> a
minimum x = fold min x
{-# INLINE minimum #-}

toList ::  (Fold v a) => v -> [a]
toList = foldr (:) [] 
{-# INLINE toList #-}







-- * Matrix Types

type Mat22 a = Vec2 (Vec2 a)
type Mat23 a = Vec2 (Vec3 a)
type Mat24 a = Vec2 (Vec4 a)

type Mat32 a = Vec3 (Vec2 a)
type Mat33 a = Vec3 (Vec3 a)
type Mat34 a = Vec3 (Vec4 a)
type Mat35 a = Vec3 (Vec5 a)
type Mat36 a = Vec3 (Vec6 a)

type Mat42 a = Vec4 (Vec2 a)
type Mat43 a = Vec4 (Vec3 a)
type Mat44 a = Vec4 (Vec4 a)
type Mat45 a = Vec4 (Vec5 a)
type Mat46 a = Vec4 (Vec6 a)
type Mat47 a = Vec4 (Vec7 a)
type Mat48 a = Vec4 (Vec8 a)

-- | convert a matrix to a list-of-lists
matToLists ::  (Fold v a, Fold m v) => m -> [[a]]
matToLists   = (P.map toList) . toList
{-# INLINE matToLists   #-}

-- | convert a matrix to a list in row-major order
matToList  ::  (Fold v a, Fold m v) => m -> [a]
matToList    = concat . matToLists
{-# INLINE matToList    #-}

-- | convert a list-of-lists into a matrix
matFromLists :: (Vec j a v, Vec i v m, VecList a v, VecList v m) => [[a]] -> m
matFromLists = fromList . (P.map fromList)
{-# INLINE matFromLists #-}

-- | convert a list into a matrix. (row-major order)
matFromList :: forall i j v m a. (Vec i v m, Vec j a v, Nat i, VecList a v, VecList v m) => [a] -> m
matFromList  = matFromLists . groupsOf (nat(undefined::i))
  where groupsOf n xs = let (a,b) = splitAt n xs in a:(groupsOf n b)
{-# INLINE matFromList  #-}




-- Storable instances. 

instance Storable a => Storable (a:.()) where
  sizeOf _ = sizeOf (undefined::a)
  alignment _ = alignment (undefined::a)
  peek p = peek (castPtr p) >>= \a -> return (a:.())
  peekByteOff p o = peek (p`plusPtr`o)
  peekElemOff p i = peek (p`plusPtr`(i*sizeOf(undefined::a)))
  poke p (a:._) = poke (castPtr p) a
  pokeByteOff p o x = poke (p`plusPtr`o) x
  pokeElemOff p i x = poke (p`plusPtr`(i*sizeOf(undefined::a))) x
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
  peek p = 
    peek (castPtr p) >>= \a -> 
    peek (castPtr (p`plusPtr`sizeOf(undefined::a))) >>= \v -> 
    return (a:.v)
  peekByteOff p o = peek (p`plusPtr`o)
  peekElemOff p i = peek (p`plusPtr`(i*sizeOf(undefined::(a:.a:.v))))
  poke p (a:.v) = 
    poke (castPtr p) a >> 
    poke (castPtr (p`plusPtr`sizeOf(undefined::a))) v
  pokeByteOff p o x = poke (p`plusPtr`o) x
  pokeElemOff p i x = poke (p`plusPtr`(i*sizeOf(undefined::(a:.a:.v)))) x
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
-- The rule is : 
--    If the method is unary, it's a map.  
--    If it's binary, it's a zipWith.

instance
    (Eq u, ShowVec u, Num a
    ,Map a a (a:.u) (a:.u) 
    ,ZipWith a a a (a:.u) (a:.u) (a:.u)
    ,Vec (Succ l) a (a:.u)
    )
    => Num (a:.u) 
  where
    (+) u v = zipWith (+) u v 
    (-) u v = zipWith (-) u v
    (*) u v = zipWith (*) u v
    abs u = map abs u
    signum u = map signum u
    fromInteger i = vec (fromInteger i)
    {-# INLINE (+) #-}
    {-# INLINE (-) #-}
    {-# INLINE (*) #-}
    {-# INLINE abs #-}
    {-# INLINE signum #-}
    {-# INLINE fromInteger #-}


instance 
    (Eq u, ShowVec u, Fractional a
    ,Ord (a:.u)
    ,ZipWith a a a (a:.u) (a:.u) (a:.u)
    ,Map a a (a:.u) (a:.u)
    ,Vec (Succ l) a (a:.u)
    ,Show (a:.u)
    ) 
    => Fractional (a:.u) 
  where
    (/) u v = zipWith (/) u v
    recip u = map recip u
    fromRational r = vec (fromRational r)
    {-# INLINE (/) #-}
    {-# INLINE recip #-}
    {-# INLINE fromRational #-}




--- UArray instances

sizeOf# :: Storable a => a -> Int#
sizeOf# x = case sizeOf x of I# n# -> n#


-- primitive array reading/writing for vectors
class VecArrayRW v where
    vaRead#   :: MutableByteArray# s# -> Int# -> State# s# -> (# State# s#, v #)
    vaWrite#  :: MutableByteArray# s# -> Int# -> v -> State# s# -> State# s#
    vaIndex#  :: ByteArray# -> Int# -> v
    vaSizeOf# :: v -> Int# --the size of a vector in bytes
    vaLength# :: v -> Int# --the length of a vector 
    init#     :: v         --the default item when newArray_ is used


instance VecArrayRW (Int:.()) where
    vaRead# arr# i# s1# =
        case readIntArray# arr# i# s1# of 
          (# s2#, x# #) -> (# s2#, ((I# x#):.()) #) 
    vaWrite# arr# i# ((I# x#):._) s1# = 
        case writeIntArray# arr# i# x# s1# of { s2# -> s2# }
    vaIndex# arr# i# = I# (indexIntArray# arr# i#) :. ()
    vaSizeOf# _ = sizeOf# (undefined::Int)
    vaLength# _ = 1#
    init# = 0:.()
    {-# INLINE vaRead# #-}
    {-# INLINE vaWrite# #-}
    {-# INLINE vaIndex# #-}
    {-# INLINE vaSizeOf# #-}
    {-# INLINE vaLength# #-}
    {-# INLINE init# #-}

instance (VecArrayRW (Int:.v)) => VecArrayRW (Int:.Int:.v) where
    vaRead# arr# i# s1# = 
        case readIntArray# arr# i# s1# of { (# s2#, x# #) -> 
        case vaRead# arr# (i# +# 1#) s2# of { (# s3#, v  #) -> 
        (# s3#, ((I# x#):.v) #) }}
    vaWrite# arr# i# ((I# x#):.v) s1# = 
        case writeIntArray# arr# i# x# s1# of { s2# -> 
        case vaWrite# arr# (i# +# 1#) v s2# of { s3# -> s3# }}
    vaIndex# arr# i# = I# (indexIntArray# arr# i#) :. 
                       vaIndex# arr# (i# +# 1#)
    vaSizeOf# _ = sizeOf# (undefined::Int) +# vaSizeOf# (undefined::Int:.v)
    vaLength# _ = 1# +# vaLength# (undefined::Int:.v)
    init# = 0 :. init# 
    {-# INLINE vaRead# #-}
    {-# INLINE vaWrite# #-}
    {-# INLINE vaIndex# #-}
    {-# INLINE vaSizeOf# #-}
    {-# INLINE vaLength# #-}
    {-# INLINE init# #-}

instance VecArrayRW (Double:.()) where
    vaRead# arr# i# s1# =
        case readDoubleArray# arr# i# s1# of 
          (# s2#, x# #) -> (# s2#, ((D# x#):.()) #) 
    vaWrite# arr# i# ((D# x#):._) s1# = 
        case writeDoubleArray# arr# i# x# s1# of { s2# -> s2# }
    vaIndex# arr# i# = D# (indexDoubleArray# arr# i#) :. ()
    vaSizeOf# _ = sizeOf# (undefined::Double)
    vaLength# _ = 1#
    init# = 0:.()
    {-# INLINE vaRead# #-}
    {-# INLINE vaWrite# #-}
    {-# INLINE vaIndex# #-}
    {-# INLINE vaSizeOf# #-}
    {-# INLINE vaLength# #-}
    {-# INLINE init# #-}

instance (VecArrayRW (Double:.v)) => VecArrayRW (Double:.Double:.v) where
    vaRead# arr# i# s1# = 
        case readDoubleArray# arr# i# s1# of { (# s2#, x# #) -> 
        case vaRead# arr# (i# +# 1#) s2# of { (# s3#, v  #) -> 
        (# s3#, ((D# x#):.v) #) }}
    vaWrite# arr# i# ((D# x#):.v) s1# = 
        case writeDoubleArray# arr# i# x# s1# of { s2# -> 
        case vaWrite# arr# (i# +# 1#) v s2# of { s3# -> s3# }}
    vaIndex# arr# i# = D# (indexDoubleArray# arr# i#) :. 
                       vaIndex# arr# (i# +# 1#)
    vaSizeOf# _ = sizeOf# (undefined::Double) +# vaSizeOf# (undefined::Double:.v)
    vaLength# _ = 1# +# vaLength# (undefined::Double:.v)
    init# = 0 :. init# 
    {-# INLINE vaRead# #-}
    {-# INLINE vaWrite# #-}
    {-# INLINE vaIndex# #-}
    {-# INLINE vaSizeOf# #-}
    {-# INLINE vaLength# #-}
    {-# INLINE init# #-}


instance VecArrayRW (Float:.()) where
    vaRead# arr# i# s1# =
        case readFloatArray# arr# i# s1# of 
          (# s2#, x# #) -> (# s2#, ((F# x#):.()) #) 
    vaWrite# arr# i# ((F# x#):._) s1# = 
        case writeFloatArray# arr# i# x# s1# of { s2# -> s2# }
    vaIndex# arr# i# = F# (indexFloatArray# arr# i#) :. ()
    vaSizeOf# _ = sizeOf# (undefined::Float)
    vaLength# _ = 1#
    init# = 0:.()
    {-# INLINE vaRead# #-}
    {-# INLINE vaWrite# #-}
    {-# INLINE vaIndex# #-}
    {-# INLINE vaSizeOf# #-}
    {-# INLINE vaLength# #-}
    {-# INLINE init# #-}

instance (VecArrayRW (Float:.v)) => VecArrayRW (Float:.Float:.v) where
    vaRead# arr# i# s1# = 
        case readFloatArray# arr# i# s1# of { (# s2#, x# #) -> 
        case vaRead# arr# (i# +# 1#) s2# of { (# s3#, v  #) -> 
        (# s3#, ((F# x#):.v) #) }}
    vaWrite# arr# i# ((F# x#):.v) s1# = 
        case writeFloatArray# arr# i# x# s1# of { s2# -> 
        case vaWrite# arr# (i# +# 1#) v s2# of { s3# -> s3# }}
    vaIndex# arr# i# = F# (indexFloatArray# arr# i#) :. 
                       vaIndex# arr# (i# +# 1#)
    vaSizeOf# _ = sizeOf# (undefined::Float) +# vaSizeOf# (undefined::Float:.v)
    vaLength# _ = 1# +# vaLength# (undefined::Float:.v)
    init# = 0 :. init# 
    {-# INLINE vaRead# #-}
    {-# INLINE vaWrite# #-}
    {-# INLINE vaIndex# #-}
    {-# INLINE vaSizeOf# #-}
    {-# INLINE vaLength# #-}
    {-# INLINE init# #-}


instance VecArrayRW (Word8:.()) where
    vaRead# arr# i# s1# =
        case readWord8Array# arr# i# s1# of 
          (# s2#, x# #) -> (# s2#, ((W8# x#):.()) #) 
    vaWrite# arr# i# ((W8# x#):._) s1# = 
        case writeWord8Array# arr# i# x# s1# of { s2# -> s2# }
    vaIndex# arr# i# = W8# (indexWord8Array# arr# i#) :. ()
    vaSizeOf# _ = sizeOf# (undefined::Word8)
    vaLength# _ = 1#
    init# = 0:.()
    {-# INLINE vaRead# #-}
    {-# INLINE vaWrite# #-}
    {-# INLINE vaIndex# #-}
    {-# INLINE vaSizeOf# #-}
    {-# INLINE vaLength# #-}
    {-# INLINE init# #-}

instance (VecArrayRW (Word8:.v)) => VecArrayRW (Word8:.Word8:.v) where
    vaRead# arr# i# s1# = 
        case readWord8Array# arr# i# s1# of { (# s2#, x# #) -> 
        case vaRead# arr# (i# +# 1#) s2# of { (# s3#, v  #) -> 
        (# s3#, ((W8# x#):.v) #) }}
    vaWrite# arr# i# ((W8# x#):.v) s1# = 
        case writeWord8Array# arr# i# x# s1# of { s2# -> 
        case vaWrite# arr# (i# +# 1#) v s2# of { s3# -> s3# }}
    vaIndex# arr# i# = W8# (indexWord8Array# arr# i#) :. 
                       vaIndex# arr# (i# +# 1#)
    vaSizeOf# _ = sizeOf# (undefined::Word8) +# vaSizeOf# (undefined::Word8:.v)
    vaLength# _ = 1# +# vaLength# (undefined::Word8:.v)
    init# = 0 :. init# 
    {-# INLINE vaRead# #-}
    {-# INLINE vaWrite# #-}
    {-# INLINE vaIndex# #-}
    {-# INLINE vaSizeOf# #-}
    {-# INLINE vaLength# #-}
    {-# INLINE init# #-}





instance VecArrayRW (a:.v) => MArray (STUArray s) (a:.v) (ST s) where
    {-# INLINE getBounds #-}
    getBounds (STUArray l u _ _) = return (l,u)
    {-# INLINE getNumElements #-}
    getNumElements (STUArray _ _ n _) = return n
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ (l,u) = 
        unsafeNewArraySTUArray_ (l,u) (\x# -> x# *# vaSizeOf# (undefined::a:.v) )
    {-# INLINE newArray_ #-}
    newArray_ arrBounds = Array.newArray arrBounds init#
    {-# INLINE unsafeRead #-}
    unsafeRead (STUArray _ _ _ marr#) (I# i#) = ST $ \s1# -> 
        vaRead# marr# (vaLength# (undefined::a:.v) *# i#) s1# 
    {-# INLINE unsafeWrite #-}
    unsafeWrite (STUArray _ _ _ marr#) (I# i#) v = ST $ \s1# ->
        case vaWrite# marr# (vaLength# (undefined::a:.v) *# i#) v s1# of s2# -> (# s2#, () #) 

instance VecArrayRW (a:.v) => IArray UArray (a:.v) where
    {-# INLINE bounds #-}
    bounds (UArray l u _ _) = (l,u)
    {-# INLINE numElements #-}
    numElements (UArray _ _ n _) = n
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies init# )
    {-# INLINE unsafeAt #-}
    unsafeAt (UArray _ _ _ arr#) (I# i#) = 
        vaIndex# arr# (vaLength# (undefined::a:.v) *# i#)
    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f initialValue lu ies = 
        runST (unsafeAccumArrayUArray f initialValue lu ies)

