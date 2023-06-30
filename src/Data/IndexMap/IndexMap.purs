module Data.IndexMap
  ( class HasIndex
  , IndexMap
  , _IxMap
  , _at
  , fromList
  , toList
  , fromFoldable
  , toUnfoldable
  , fromFoldableWithIndex
  , empty
  , size
  , indexes
  , insert
  , singleton
  , modify
  , modifyM
  , foldM
  , vectIso
  , vectIsoKeyed
  ) where

import Prelude

import Data.FoldableWithIndex (class FoldableWithIndex, foldrWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Lens (Getter', Lens', lens)
import Data.Lens.Getter (to)
import Data.Maybe (Maybe(..), fromJust)
import Data.Reflectable (class Reflectable)
import Data.Traversable (class Foldable, class Traversable, foldr)
import Data.TraversableWithIndex (class TraversableWithIndex)
import Data.Tuple (Tuple(..), uncurry)
import Data.Profunctor (dimap)
import Data.Unfoldable (class Unfoldable)
import Data.Vector (class NonNegative, toInt)
import Data.Vector as V
import Erl.Data.List (List)
import Erl.Data.List as List
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Partial.Unsafe (unsafePartial)
import Prim.Int (class Add, class Compare)
import Prim.Ordering (LT)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

----------------------------------------------------------------------

class
  ( Reflectable i Int
  , NonNegative i
  , Compare i s LT
  ) ⇐
  HasIndex i s

----------------------------------------------------------------------

-- | `IndexMap` is intended to be closer in semantics to linked
-- | lists rathen than to maps, but offering faster random access.
-- | Unlike regular maps, random deletion and insertion is not
-- | supported, and insertion and deletion at the end/start
-- | is costly. Thus, this data structure is to be used mostly
-- | in situations which require cheap and index-safe random access
-- | and very little or no updates at all, but on a level higher
-- | than ETS/PTS/Refs.
newtype IndexMap ∷ Int → Type → Type
newtype IndexMap s a = IndexMap (Map Int a)

instance Show a ⇒ Show (IndexMap s a) where
  show (IndexMap m) = "(IndexMap" <> show m <> ")"

_IxMap ∷ ∀ s a. Getter' (IndexMap s a) (Map Int a)
_IxMap = to \(IndexMap m) → m

_at ∷ ∀ s a i. HasIndex i s ⇒ Proxy i → Lens' (IndexMap s a) a
_at i = lens (index i) (\m v → modify (const v) i m)

----------------------------------------------------------------------
-- Conversions

fromList ∷ ∀ s a. NonNegative s ⇒ List a → Maybe (IndexMap s a)
fromList xs | List.length xs == toInt (Proxy ∷ _ s) =
  Just $ IndexMap $ foldrWithIndex Map.insert Map.empty xs
fromList _ = Nothing

toList ∷ ∀ s a. NonNegative s ⇒ IndexMap s a → List a
toList (IndexMap xs) = Map.values xs

vectIso
  ∷ ∀ s a b
  . NonNegative s
  ⇒ (V.Vector s a → V.Vector s b)
  → (IndexMap s a → IndexMap s b)
vectIso = dimap toVect fromVect

  where
  toVect (IndexMap m) = unsafeCoerce $ Map.values m
  fromVect v = unsafePartial $ fromJust $ fromList (unsafeCoerce v)

vectIsoKeyed
  ∷ ∀ s a b
  . (V.Vector s (Tuple Int a) → V.Vector s (Tuple Int b))
  → (IndexMap s a → IndexMap s b)
vectIsoKeyed = dimap toVect fromVect

  where
  toVect (IndexMap m) = unsafeCoerce $ (Map.toUnfoldable m :: List _)
  fromVect v = IndexMap $ foldr (uncurry Map.insert) Map.empty v

fromFoldable
  ∷ ∀ s a f. NonNegative s ⇒ Foldable f ⇒ f a → Maybe (IndexMap s a)
fromFoldable xs =
  foldr folder (Tuple 0 Map.empty) xs
    # \(Tuple size' result) →
        if size' == toInt (Proxy ∷ _ s) then Just $ IndexMap result
        else Nothing

  where
  folder x (Tuple n acc) = Tuple (n + 1) $ Map.insert n x acc

toUnfoldable ∷ ∀ s f a. Unfoldable f ⇒ IndexMap s a → f (Tuple Int a)
toUnfoldable (IndexMap m) = Map.toUnfoldable m

fromFoldableWithIndex
  ∷ ∀ s a f
  . NonNegative s
  ⇒ FoldableWithIndex Int f
  ⇒ (f a → Maybe (IndexMap s a))
fromFoldableWithIndex =
  foldrWithIndex folder (Just $ Tuple 0 Map.empty)
    >=> \(Tuple _ res) →
      if Map.size res == toInt (Proxy ∷ _ s) then
        pure (IndexMap res)
      else Nothing

  where
  folder i v (Just (Tuple expected acc)) =
    if i == expected then
      Just $ Tuple (expected + 1) $ Map.insert i v acc
    else Nothing
  folder _ _ Nothing = Nothing

----------------------------------------------------------------------
-- Basic operations

empty ∷ ∀ a. IndexMap 0 a
empty = IndexMap Map.empty

size ∷ ∀ s a. NonNegative s ⇒ IndexMap s a → Int
size _ = toInt (Proxy ∷ _ s)

indexes ∷ ∀ s a. IndexMap s a → List Int
indexes (IndexMap m) = Map.keys m

insert
  ∷ ∀ s a s'
  . NonNegative s
  ⇒ Reflectable s' Int
  ⇒ Add s 1 s'
  ⇒ (a → IndexMap s a → IndexMap s' a)
insert x (IndexMap xs) =
  IndexMap $ Map.insert (toInt (Proxy ∷ _ s')) x xs

singleton ∷ ∀ a. a → IndexMap 1 a
singleton x = IndexMap $ Map.singleton 0 x

index ∷ ∀ s a i. HasIndex i s ⇒ Proxy i → IndexMap s a → a
index i (IndexMap m) = indexImpl (toInt i) m

foreign import indexImpl ∷ ∀ a. Int → Map Int a → a

modify
  ∷ ∀ s a i
  . HasIndex i s
  ⇒ (a → a)
  → Proxy i
  → IndexMap s a
  → IndexMap s a
modify f i (IndexMap m) = IndexMap $ modifyImpl (toInt i) f m

foreign import modifyImpl ∷ ∀ a b. a → (b → b) → Map a b → Map a b

modifyM
  ∷ ∀ s a i m
  . HasIndex i s
  ⇒ Functor m
  ⇒ (a → m a)
  → Proxy i
  → IndexMap s a
  → m (IndexMap s a)
modifyM f i m = f (index i m) <#> \v → modify (const v) i m

foldM
  ∷ ∀ s a m z. Monad m ⇒ (z → Int → a → m z) → z → IndexMap s a → m z
foldM f z (IndexMap m) = Map.foldM f z m

----------------------------------------------------------------------
-- Instances

derive newtype instance Eq a ⇒ Eq (IndexMap s a)
derive newtype instance Ord a ⇒ Ord (IndexMap s a)
derive newtype instance Functor (IndexMap s)
derive newtype instance FunctorWithIndex Int (IndexMap s)
derive newtype instance Foldable (IndexMap s)
derive newtype instance FoldableWithIndex Int (IndexMap s)
derive newtype instance Traversable (IndexMap s)
derive newtype instance TraversableWithIndex Int (IndexMap s)

instance Apply (IndexMap s) where
  apply (IndexMap mfs) (IndexMap mxs) = IndexMap
    $ Map.fromFoldable
    $ List.zipWith flap (Map.toUnfoldable mfs) (Map.values mxs)

instance NonNegative s ⇒ Applicative (IndexMap s) where
  pure x = (V.replicate x ∷ _ s _)
    # \v → unsafePartial $ fromJust $ fromList $ unsafeCoerce v

