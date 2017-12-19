{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

import Cipher (alphabet, decode, encode)
import Chapter11
  ( BinaryTree(Leaf,Node)
  , foldTree
  , inorder
  , insert'
  , mapTree
  , postorder
  , preorder
  )
import Chapter15 (Optional(Nada,Only), XOR(..))

import Data.Monoid ((<>), Product(..), Sum(..))

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), Assertion, assertBool, testCase)
import Test.Tasty.SmallCheck (testProperty)
import Test.SmallCheck.Series (Serial, newtypeCons, series)

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps]

newtype Sum' a = Sum' a
  deriving (Eq, Show)

instance Num a => Monoid (Sum' a) where
    mempty = Sum' 0
    mappend (Sum' x) (Sum' y) = Sum' (x + y)

instance Serial m a => Serial m (Sum' a) where
    series = newtypeCons Sum'

type OptSum = Optional (Sum' Int)

scProps :: TestTree
scProps = testGroup "SmallCheck properties"
  [ testGroup "Monoidal laws for Optional"
    [ testProperty "mempty <> x == x" $ \x -> (mempty :: OptSum) <> x == x
    , testProperty "x <> mempty == x" $ \x -> x <> (mempty :: OptSum) == x
    , testProperty "(x <> y) <> z == x <> (y <> z)" $ \x y z ->
            (x <> y) <> z == (x :: OptSum) <> (y <> z)
    ]
  , testGroup "Monoidal laws for XOR"
    [ testProperty "mempty <> x == x" $ \x -> (mempty :: XOR) <> x == x
    , testProperty "x <> mempty == x" $ \x -> x <> (mempty :: XOR) == x
    , testProperty "(x <> y) <> z == x <> (y <> z)" $ \x y z ->
            (x <> y) <> z == (x :: XOR) <> (y <> z)
    ]
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "Chapter 11" testCh11
  , testCase "Chapter 15" testCh15
  , testCase "Cipher" testCipher
  ]

testCh11 :: Assertion
testCh11 = do
    let t1 :: BinaryTree Int
        t1 = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)
        t1' = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)
    mapTree (+1) t1 @?= t1'

    let t2 :: BinaryTree Int
        t2 = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)
    preorder t2 @?= [2, 1, 3]
    inorder t2 @?= [1, 2, 3]
    postorder t2 @?= [1, 3, 2]

    let t3 :: BinaryTree Int
        t3 = let i = insert' in i 10 $ i 0 $ i 2 $ i 4 $ i 6 Leaf
    foldTree (+) 0 t3 @?= 22
    foldTree (:) [] t3 @?= [0, 2, 4, 6, 10]

testCh15 :: Assertion
testCh15 = do
    Only (Sum (1 :: Int)) <> Only (Sum 1) @?= Only (Sum 2)
    Only (Product (4 :: Int)) <> Only (Product 2) @?= Only (Product 8)
    Only (Sum (1 :: Int)) <> Nada @?= Only (Sum 1)
    Nada <> Only [1 :: Int] @?= Only [1]

testCipher :: Assertion
testCipher = assertBool "codec" $ and [ decode (encode p k) k == p
                                      | p <- alphabet, k <- alphabet ]

main :: IO ()
main = defaultMain tests
