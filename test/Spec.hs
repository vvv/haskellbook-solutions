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

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), Assertion, assertBool, testCase)

tests :: TestTree
tests = testGroup "Unit tests"
  [ testCase "Chapter 11" testCh11
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

testCipher :: Assertion
testCipher = assertBool "codec" $ and [ decode (encode p k) k == p
                                      | p <- alphabet, k <- alphabet ]

main :: IO ()
main = defaultMain tests
