module Test.Data.Vector where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (isJust, Maybe(..))
import Data.Reflectable (reflectType)
import Data.Vector ((:+))
import Data.Vector as V
import Effect.Console (log)
import Erl.Data.List as List
import Erl.Data.List.NonEmpty as NEL
import Erl.Test.EUnit (TestF, suite, test)
import Test.Assert (assert, assertEqual)
import Type.Proxy (Proxy(..))

main :: Free TestF Unit
main = do

  let assertJust = assert <<< isJust
  let list5 = List.fromFoldable [ 1, 2, 3, 4, 5 ]
  let maybevec5 = V.fromList list5 :: _ (_ 5 _)

  suite "Vectors -- conversions" do

    test "Lists and other collections convert to vectors" do
      assertJust $ (V.fromList list5 :: _ (_ 5 _))
      assertJust $ (V.fromArray [ 1, 2, 3, 4, 5 ] :: _ (_ 5 _))
      assertJust $ (V.fromFoldable [ 1, 2, 3, 4, 5 ] :: _ (_ 5 _))
      log "Conversion from: test passed"

    test "Vectors convert to lists and other collections" do
      assertEqual
        { actual: V.toList <$> maybevec5, expected: Just list5 }
      assertEqual
        { actual: V.toNonEmptyList <$> maybevec5
        , expected: NEL.fromList list5
        }
      assertEqual
        { actual: V.toArray <$> maybevec5
        , expected: Just [ 1, 2, 3, 4, 5 ]
        }
      log "Conversion to: test passed"

  suite "Vectors -- basics" do

    test "Size and singleton/empty" do
      assert $ Just 5 == (V.size <$> maybevec5)
      assert $ 0 == V.size (V.empty :: _ _ Int)
      assert $ 1 == V.size (V.singleton 123)
      log "Size and singleton/empty tests passed"

    test "Size and cons/uncons" do
      assert $ 2 == V.size (V.cons 999 $ V.singleton 111)
      assert $ 3 == V.size (1 :+ 2 :+ 3 :+ V.empty)
      let { head, tail } = V.uncons (3 :+ 4 :+ 5 :+ V.empty)
      assertEqual { actual: V.size tail, expected: 2 }
      assert $ head == 3
      log "Size and cons/uncons tests passed"

    test "Size and snoc/unsnoc" do
      assert $ 999 == V.last (V.singleton 111 `V.snoc` 999)
      assert $ 2 == V.size (V.tail (1 :+ 2 :+ 3 :+ V.empty))
      let { init, last } = V.unsnoc (3 :+ 4 :+ 5 :+ V.empty)
      assertEqual { actual: V.size init, expected: 2 }
      assert $ last == 5
      log "Size and cons/uncons tests passed"

    test "Indexing" do
      let vec = 1 :+ 2 :+ 3 :+ 4 :+ V.empty
      assert $ vec V.!! (Proxy :: _ 0) == 1
      assert $ vec V.!! (Proxy :: _ 1) == 2
      assert $ vec V.!! (Proxy :: _ 2) == 3
      log "Indexing tests passed"

  suite "Vectors - other functions" do

    test "Ranges" do
      assertEqual
        { actual: V.range' :: _ 3 _
        , expected: 0 :+ 1 :+ 2 :+ V.empty
        }
      assertEqual
        { actual: V.range (Proxy :: _ 10) (Proxy :: _ 12)
        , expected: 10 :+ 11 :+ 12 :+ V.empty
        }
      assertEqual
        { actual: V.size $ V.slice (Proxy :: _ 5) (Proxy :: _ 9)
            $ V.range (Proxy :: _ 0) (Proxy :: _ 20)
        , expected: 4
        }
      assertEqual
        { actual: V.size $ V.drop (Proxy :: _ 11)
            $ V.range (Proxy :: _ 0) (Proxy :: _ 100)
        , expected: 90
        }
      assertEqual
        { actual: V.size $ V.take (Proxy :: _ 10)
            $ V.range (Proxy :: _ 10) (Proxy :: _ 30)
        , expected: 10
        }

    test "Zippers" do
      let xs = 1 :+ 2 :+ 3 :+ V.empty
      let ys = 10 :+ 20 :+ 30 :+ V.empty
      let zippedMul = 10 :+ 40 :+ 90 :+ V.empty
      assert $ zippedMul == V.zipWith (*) xs ys

    test "Modifying values based on an index" do
      let vec = "One" :+ "Two" :+ "Three" :+ V.empty
      let vec' = "One" :+ "One and a half" :+ "Two" :+ "Three" :+ V.empty
      let vec'' = "One" :+ "One and no half" :+ "Two" :+ "Three" :+ V.empty
      assert $ vec' == V.insertAt (Proxy :: _ 1) "One and a half" vec
      assert $ vec == V.deleteAt (Proxy :: _ 1) vec'
      assert $ vec'' == V.modifyAt (Proxy :: _ 1) (const "One and no half") vec'

    test "Sorting" do
      let unsorted = 3 :+ 2 :+ 4 :+ 5 :+ 1 :+ V.empty
      let sorted = 1 :+ 2 :+ 3 :+ 4 :+ 5 :+ V.empty
      assert $ sorted == V.sort unsorted
      assert $ sorted == V.sortBy (comparing identity) unsorted

    test "Generating" do
      let negated = 0 :+ (-1) :+ (-2) :+ (-3) :+ (-4) :+ V.empty
      assertEqual
        { actual: (V.generate \i -> negate i) :: _ 5 _
        , expected: negated
        }
      assertEqual
        { actual: (V.generate' \i -> negate $ reflectType i) :: _ 5 _
        , expected: negated
        }

  suite "Vectors -- instances" do

    test "Functor" do
      let before = 1 :+ 2 :+ 3 :+ 4 :+ V.empty
      let notComposed = (_ <> "a") <$> show <$> before
      let composed = ((_ <> "a") <<< show) <$> before
      assert $ notComposed == composed
      assert $ before == identity `map` before

    test "Apply" do
      let expected = 10 :+ 20 :+ 33 :+ V.empty
      let fs = (const 10) :+ (add 15) :+ (mul 3) :+ V.empty
      let xs = 999 :+ 5 :+ 11 :+ V.empty
      assert $ expected == (fs <*> xs)

    test "Applicative" do
      let expected = 1 :+ 1 :+ 1 :+ 1 :+ V.empty
      assert $ eq expected $ (pure 1 :: _ 4 _)

