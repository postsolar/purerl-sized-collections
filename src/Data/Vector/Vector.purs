module Data.Vector
  ( class NonNegative
  , class Positive
  , class Sub
  , class IndexVector
  , toInt
  , Vector
  , _V
  , _V'
  , fromList
  , toList
  , toNonEmptyList
  , fromArray
  , toArray
  , fromFoldable
  , toUnfoldable
  , size
  , singleton
  , empty
  , cons
  , (:+)
  , uncons
  , first
  , tail
  , snoc
  , unsnoc
  , last
  , init
  , index
  , (!!)
  , reverse
  , range'
  , range
  , (..)
  , slice
  , take
  , drop
  , zipWith
  , zipWithA
  , zip
  , unzip
  , insertAt
  , deleteAt
  , updateAt
  , modifyAt
  , sort
  , sortBy
  , elemIndex
  , elemLastIndex
  , generate
  , generate'
  ) where

import Prelude

import Control.Apply (lift2)
import Data.Array as Array
import Data.Distributive (class Distributive, collectDefault, distribute)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldl)
import Data.FoldableWithIndex (class FoldableWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Lens (Getter', Prism', prism', view)
import Data.Lens.AffineTraversal (affineTraversal)
import Data.Lens.Getter (to)
import Data.Lens.Index (class Index)
import Data.Maybe (Maybe(..), fromJust)
import Data.Profunctor (dimap)
import Data.Profunctor.Strong ((***))
import Data.Reflectable (class Reflectable, reflectType)
import Data.Semigroup.Foldable (class Foldable1, foldMap1, foldl1, foldr1)
import Data.Semigroup.Traversable (class Traversable1, sequence1Default, traverse1)
import Data.Traversable (class Traversable)
import Data.TraversableWithIndex (class TraversableWithIndex)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable)
import Erl.Data.List (List)
import Erl.Data.List as List
import Erl.Data.List.NonEmpty (NonEmptyList)
import Erl.Data.List.NonEmpty as NEL
import Partial.Unsafe (unsafePartial)
import Prim.Int (class Add, class Compare)
import Prim.Ordering (GT, LT)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

----------------------------------------------------------------------
-- Helper classes

class (Reflectable n Int, Compare n (-1) GT) ⇐ NonNegative n
class (Reflectable n Int, Compare n 0 GT) ⇐ Positive n
class Add x1 x2 sum ⇐ Sub sum x1 x2
class
  ( NonNegative i
  , NonNegative s
  , Compare i s LT
  ) ⇐
  IndexVector i s

instance (Reflectable n Int, Compare n (-1) GT) ⇒ NonNegative n
instance (Reflectable n Int, Compare n 0 GT) ⇒ Positive n
instance Add x1 x2 sum ⇒ Sub sum x1 x2
instance
  ( NonNegative i
  , NonNegative s
  , Compare i s LT
  ) ⇒
  IndexVector i s

toInt ∷ ∀ n. Reflectable n Int ⇒ Proxy n → Int
toInt = reflectType

----------------------------------------------------------------------

-- | `Vector` is a newtype over `Erl.Data.List` with its length
-- | known at compile time, which makes some operations easier
-- | due to not having to deal with `Maybe`'s and some operations
-- | impossible due to length information getting lost.
-- | All indexing is 0-based.
newtype Vector ∷ Int → Type → Type
newtype Vector s a = Vector (List a)

instance (NonNegative s, Show a) ⇒ Show (Vector s a) where
  show (Vector xs) = "(Vector " <> show xs <> ")"

_V ∷ ∀ s a. NonNegative s ⇒ Prism' (List a) (Vector s a)
_V = prism' (view _V') fromList

_V' ∷ ∀ s a. Getter' (Vector s a) (List a)
_V' = to \(Vector xs) → xs

instance IndexVector i s ⇒ Index (Vector s a) (Proxy i) a where
  ix i = affineTraversal set pre
    where
    set ∷ Vector s a → a → Vector s a
    set s b = updateAt i b s

    pre ∷ Vector s a → Either (Vector s a) a
    pre s = Right $ index s i

----------------------------------------------------------------------
-- Conversions

-- | Attempts to convert an Erlang list to a Vector of a size
-- | specified at the type level.
-- | Running time: O(n) due to verifying the length of the input list.
fromList ∷ ∀ s a. NonNegative s ⇒ List a → Maybe (Vector s a)
fromList xs
  | List.length xs == toInt (Proxy ∷ _ s) = Just (Vector xs)
  | otherwise = Nothing

-- | Converts a vector to an Erlang list, while losing its size
-- | information at the type level.
-- | Running time: no-op
toList ∷ ∀ s a. NonNegative s ⇒ Vector s a → List a
toList (Vector xs) = xs

-- | Converts a vector to a NonEmptyList Erlang list, while losing
-- | its size information at the type level.
-- | Running time: O(1)
toNonEmptyList ∷ ∀ s a. Positive s ⇒ Vector s a → NonEmptyList a
toNonEmptyList (Vector xs) =
  unsafePartial $ fromJust $ NEL.fromList xs

-- | Attempts to convert an array to a Vector of a size
-- | specified at the type level.
-- | Running time: O(n)
fromArray ∷ ∀ s a. NonNegative s ⇒ Array a → Maybe (Vector s a)
fromArray xs
  | Array.length xs == toInt (Proxy ∷ _ s) =
      Just $ Vector $ List.fromFoldable $ xs
  | otherwise =
      Nothing

-- | Converts a vector to an array, while losing its size
-- | information at the type level.
-- | Running time: O(n)
toArray ∷ ∀ s a. NonNegative s ⇒ Vector s a → Array a
toArray (Vector xs) = Array.fromFoldable xs

-- | Converts any foldable structure into a sized vector.
-- | Running time: O(n)
fromFoldable
  ∷ ∀ f s a. NonNegative s ⇒ Foldable f ⇒ f a → Maybe (Vector s a)
fromFoldable xs = case foldl folder (Tuple 0 mempty) xs of
  Tuple n xs' | n == toInt (Proxy ∷ _ s) →
    Just $ Vector $ List.reverse xs'
  _ →
    Nothing

  where
  folder (Tuple s elems) elem =
    Tuple (s + 1) (elem `List.cons` elems)

-- | Converts a vector into any unfoldable structure,
-- | while losing its size information at the type level.
-- | Running time: O(n)
toUnfoldable ∷ ∀ s f a. Unfoldable f ⇒ Vector s a → f a
toUnfoldable (Vector xs) = List.toUnfoldable xs

----------------------------------------------------------------------
-- Basic operations

-- | Given a vector, returns its length as a value-level integer.
size ∷ ∀ s a. Reflectable s Int ⇒ Vector s a → Int
size _ = toInt (Proxy ∷ _ s)

-- | Creates a vector with one element.
singleton ∷ ∀ a. a → Vector 1 a
singleton = Vector <<< List.singleton

-- | Creates an empty vector (sized 0).
empty ∷ ∀ a. Vector 0 a
empty = Vector List.nil

-- | Prepends an element to a vector while incrementing the vector's
-- | type-level size.
cons ∷ ∀ s s' a. Add s 1 s' ⇒ a → Vector s a → Vector s' a
cons x (Vector xs) = Vector $ x `List.cons` xs

infixr 6 cons as :+

-- | Partitions a vector into its first element and all other
-- | elements as a new vector with decremented size.
uncons
  ∷ ∀ s s' a
  . Positive s
  ⇒ Add s' 1 s
  ⇒ Vector s a
  → { head ∷ a, tail ∷ Vector s' a }
uncons = unconsImpl

foreign import unconsImpl
  ∷ ∀ s s' a. Vector s a → { head ∷ a, tail ∷ Vector s' a }

-- | Returns the first element of a vector given its typelevel
-- | size is greater than or equal to 1.
first ∷ ∀ s a. Positive s ⇒ Vector s a → a
first = firstImpl

foreign import firstImpl ∷ ∀ s a. Vector s a → a

tail
  ∷ ∀ s a s'
  . Positive s
  ⇒ Add s' 1 s
  ⇒ (Vector s a → Vector s' a)
tail = uncons >>> \{ tail: t } → t

-- | Appends an element to a vector while incrementing the vector's
-- | type-level size.
snoc ∷ ∀ s s' a. Add s 1 s' ⇒ Vector s a → a → Vector s' a
snoc (Vector xs) x = Vector $ List.snoc xs x

-- | Partitions a vector into its last element and all other
-- | elements as a new vector with decremented size.
unsnoc
  ∷ ∀ s s' a
  . Add s' 1 s
  ⇒ Vector s a
  → { init ∷ Vector s' a, last ∷ a }
unsnoc = unsnocImpl

foreign import unsnocImpl
  ∷ ∀ s s' a. Vector s a → { init ∷ Vector s' a, last ∷ a }

-- | Returns the first element of a vector given its typelevel
-- | size is greater than or equal to 1.
last ∷ ∀ s a. Positive s ⇒ Vector s a → a
last = lastImpl

foreign import lastImpl ∷ ∀ s a. Vector s a → a

init
  ∷ ∀ s a s'
  . Positive s
  ⇒ Add s' 1 s
  ⇒ (Vector s a → Vector s' a)
init = unsnoc >>> \{ init: i } → i

-- | Replicates a given value the specified number of times.
replicate ∷ ∀ s a. NonNegative s ⇒ a → Vector s a
replicate x = Vector $ replicateImpl (toInt (Proxy ∷ _ s)) x

foreign import replicateImpl ∷ ∀ a. Int → a → List a

-- Returns the `i`-th element of the vector.
index
  ∷ ∀ s a i
  . NonNegative s
  ⇒ NonNegative i
  ⇒ Compare i s LT
  ⇒ Vector s a
  → Proxy i
  → a
index (Vector xs) p =
  unsafePartial $ fromJust $ List.index xs (toInt p)

infixl 8 index as !!

reverse ∷ ∀ s a. NonNegative s ⇒ Vector s a → Vector s a
reverse (Vector xs) = Vector (List.reverse xs)

----------------------------------------------------------------------
-- More advanced functions

-- | Generates a range from 0 up to the vector's size exclusively:
-- | ```
-- | (range :: _ 10 _)
-- |   == (fromJust $ fromList $ List.range 0 9 :: _ 10 _)
-- | ```
range' ∷ ∀ s. NonNegative s ⇒ Vector s Int
range' = generate identity

-- | Generates a range from any non-negative type-level integer up
-- | to any positive integer, inclusively. The second integer has
-- | to be greater than the first one. The size of the resulting
-- | vector is thus `j - i + 1`.
range
  ∷ ∀ s s' i j
  . NonNegative i
  ⇒ NonNegative j
  ⇒ NonNegative s'
  ⇒ Sub j i s'
  ⇒ Add s' 1 s
  ⇒ (Proxy i → Proxy j → Vector s Int)
range i j = Vector $ List.range (toInt i) (toInt j)

infix 8 range as ..

-- | Slices a vector from start index (inclusive) to end index
-- | (non-inclusive).
-- The type sig here is a bit more complicated than it should
-- be because List.slice's last index is not inclusive, but we
-- need to be able to get the last element too.
slice
  ∷ ∀ s a i j s' s''
  . Add s 1 s'
  ⇒ IndexVector i s'
  ⇒ IndexVector j s'
  ⇒ Sub j i s''
  ⇒ (Proxy i → Proxy j → Vector s a → Vector s'' a)
slice i j (Vector xs) = Vector $ List.slice (toInt i) (toInt j) xs

-- Could expand it to be able to take the whole vec as well but
-- is there a point to this?
take
  ∷ ∀ s a n
  . IndexVector n s
  ⇒ (Proxy n → Vector s a → Vector n a)
take n (Vector xs) = Vector $ List.take (toInt n) xs

drop
  ∷ ∀ s a n n'
  . IndexVector n s
  ⇒ Sub s n n'
  ⇒ (Proxy n → Vector s a → Vector n' a)
drop n (Vector xs) = Vector $ List.drop (toInt n) xs

zipWith
  ∷ ∀ s a b c. (a → b → c) → Vector s a → Vector s b → Vector s c
zipWith f (Vector xs) (Vector ys) = Vector $ List.zipWith f xs ys

zipWithA
  ∷ ∀ s m a b c
  . Applicative m
  ⇒ ((a → b → m c) → Vector s a → Vector s b → m (Vector s c))
zipWithA f (Vector xs) (Vector ys) = Vector <$> List.zipWithA f xs ys

zip ∷ ∀ s a b. Vector s a → Vector s b → Vector s (Tuple a b)
zip = zipWith Tuple

unzip
  ∷ ∀ s a b. Vector s (Tuple a b) → Tuple (Vector s a) (Vector s b)
unzip (Vector xys) = Vector *** Vector $ List.unzip xys

-- | Inserts an element at the specified non-negative index.
-- | The maximum allowed index is the size of the original vector:
-- | ```
-- | vec = range (Proxy :: _ 0) (Proxy :: _ 3) -- Vector of size 4
-- | insertAt (Proxy :: _ 4) 999 vec -- Vector (0 : 1 : 2 : 3 : 999 : nil)
-- | ```
insertAt
  ∷ ∀ s a i s'
  . IndexVector i s'
  ⇒ Add s 1 s'
  ⇒ (Proxy i → a → Vector s a → Vector s' a)
insertAt i a (Vector as) = Vector $ unsafePartial $ fromJust $
  List.insertAt (toInt i) a as

deleteAt
  ∷ ∀ s a i s'
  . IndexVector i s'
  ⇒ Add s 1 s'
  ⇒ (Proxy i → Vector s' a → Vector s a)
deleteAt i (Vector as) = Vector $ unsafePartial $ fromJust $
  List.deleteAt (toInt i) as

updateAt
  ∷ ∀ s a i
  . IndexVector i s
  ⇒ (Proxy i → a → Vector s a → Vector s a)
updateAt i a (Vector as) = Vector $ unsafePartial $ fromJust $
  List.updateAt (toInt i) a as

modifyAt
  ∷ ∀ s a i
  . IndexVector i s
  ⇒ (Proxy i → (a → a) → Vector s a → Vector s a)
modifyAt i f (Vector as) = Vector $ unsafePartial $ fromJust $
  List.modifyAt (toInt i) f as

sort ∷ ∀ s a. Ord a ⇒ Vector s a → Vector s a
sort xs = sortBy compare xs

sortBy ∷ ∀ s a. (a → a → Ordering) → Vector s a → Vector s a
sortBy cmp (Vector xs) = Vector $ List.sortBy cmp xs

elemIndex
  ∷ ∀ s a
  . Positive s
  ⇒ Eq a
  ⇒ (a → Vector s a → Maybe Int)
elemIndex x (Vector xs) = List.elemIndex x xs

elemLastIndex
  ∷ ∀ s a
  . Positive s
  ⇒ Eq a
  ⇒ (a → Vector s a → Maybe Int)
elemLastIndex x = elemIndex x <<< reverse

-- elemIndex'
--   ∷ ∀ s a i
--   . Positive s
--   ⇒ NonNegative i
--   ⇒ Eq a
--   ⇒ (a → Vector s a → Maybe (Proxy i))
-- elemIndex' x xs = go (Proxy ∷ _ 0) $ toList xs
--   where
--   go
--     ∷ ∀ curr next
--     . Add 1 curr next
--     ⇒ Proxy curr
--     → List a
--     → Maybe (Proxy i)
--   go curr xs = case List.uncons xs of
--     Just { head: h, tail: t } →
--       if h == x then Just (unsafeCoerce curr)
--       -- TODO: Doesn't want to compile
--       else go (Proxy ∷ _ next) t
--     Nothing → Nothing

-- | Generates a `Vector` by applying a function to each of the
-- | vector's indexes.
generate ∷ ∀ s a. NonNegative s ⇒ (Int → a) → Vector s a
generate f = Vector $ f <$> List.range 0 (toInt (Proxy ∷ _ s) - 1)

-- | Generates a `Vector` of the given size by applying
-- | a function to each type level index.
generate'
  ∷ ∀ s a
  . NonNegative s
  ⇒ (∀ i. NonNegative i ⇒ Compare i s LT ⇒ Proxy i → a)
  → Vector s a
generate' f = Vector
  $ map (\i → unsafeCoerceTerm (Proxy ∷ _ s) f i)
  $ List.range 0 (toInt (Proxy ∷ _ s) - 1)

----------------------------------------------------------------------
-- Internal

-- Lifts a function that takes any type-level integer value
-- from 0 to `s - 1` into a function that takes Int.
-- Similar implementation to reifyType, but unsafe because
-- type classes such as `Compare` are not ensured (so no export).
unsafeCoerceTerm
  ∷ ∀ len a
  . Proxy len
  → ( ∀ i
      . Compare i (-1) GT
      ⇒ Compare i len LT
      ⇒ Reflectable i Int
      ⇒ Proxy i
      → a
    )
  → (Int → a)
unsafeCoerceTerm _ f i =
  -- Couldn't figure out how typeclass dicts passing works here
  -- so decided to not touch the signatures and implementation here
  internal f unit unit { reflectType: \_ → i } Proxy
  where
  internal
    ∷ ( ∀ i
        . Compare i (-1) GT
        ⇒ Compare i len LT
        ⇒ Reflectable i Int
        ⇒ Proxy i
        → a
      )
    → Unit
    → Unit
    → { reflectType ∷ Proxy _ → Int }
    → Proxy _
    → a
  internal = unsafeCoerce

----------------------------------------------------------------------
-- Instances

derive newtype instance Eq a ⇒ Eq (Vector s a)
derive newtype instance Ord a ⇒ Ord (Vector s a)
derive newtype instance Functor (Vector s)
derive newtype instance FunctorWithIndex Int (Vector s)
derive newtype instance Foldable (Vector s)
derive newtype instance FoldableWithIndex Int (Vector s)
derive newtype instance Traversable (Vector s)
derive newtype instance TraversableWithIndex Int (Vector s)

-- | Unlike those of `List` and `Array`, `Vector`'s `Apply` instance
-- | zips a vector of functions with a vector of value, applying
-- | corresponding functions and values.
instance Apply (Vector s) where
  apply (Vector fs) (Vector xs) = Vector (List.zipWith ($) fs xs)

-- | Unlike those of `List` and `Array`, `Vector`'s `Applicative`
-- | instance replicates a value `s` times, where `s` is the
-- | type-level size of the vector.
instance NonNegative s ⇒ Applicative (Vector s) where
  pure = replicate

instance NonNegative s ⇒ Distributive (Vector s) where
  distribute ∷ ∀ a g. Functor g ⇒ g (Vector s a) → Vector s (g a)
  distribute xs = generate' f
    where
    f ∷ ∀ i. NonNegative i ⇒ Compare i s LT ⇒ Proxy i → g a
    f p = flip index p <$> xs
  collect = collectDefault

instance NonNegative s ⇒ Bind (Vector s) where
  bind vec f = distribute f <*> vec

instance NonNegative s ⇒ Monad (Vector s)

instance Positive s ⇒ Foldable1 (Vector s) where
  foldMap1 f xs = foldMap1 f $ toNonEmptyList xs
  foldr1 f xs = foldr1 f $ toNonEmptyList xs
  foldl1 f xs = foldl1 f $ toNonEmptyList xs

instance
  ( Positive s
  , NonNegative s'
  , Add s' 1 s
  ) ⇒
  Traversable1 (Vector s) where
  traverse1 f = dimap toNonEmptyList (map unsafeFromNel) (traverse1 f)
    where
    -- assumes the Traversable1 instance for NonEmptyList keeps the same
    -- number of elements
    unsafeFromNel = NEL.uncons >>> \{ head, tail: t } →
      cons head (unsafePartial $ fromJust $ fromList t)

  sequence1 = sequence1Default

instance Semigroup a ⇒ Semigroup (Vector s a) where
  append = lift2 append

instance (NonNegative s, Monoid a) ⇒ Monoid (Vector s a) where
  mempty = pure mempty

instance (NonNegative s, Semiring a) ⇒ Semiring (Vector s a) where
  add = lift2 add
  zero = pure zero
  mul = lift2 mul
  one = pure one

instance (NonNegative s, Ring a) ⇒ Ring (Vector s a) where
  sub = lift2 sub

instance (NonNegative s, CommutativeRing a) ⇒ CommutativeRing (Vector s a)

