{-# OPTIONS_GHC -Wall -Werror #-}
module Chapter11 where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' x Leaf = Node Leaf x Leaf
insert' x node@(Node left y right)
  | x < y = Node (insert' x left) y right
  | x > y = Node left y (insert' x right)
  | otherwise = node

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left x right) = Node (mapTree f left) (f x) (mapTree f right)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left x right) = x : preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left x right) = preorder left ++ x : preorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left x right) = preorder left ++ preorder right ++ [x]

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ z Leaf = z
foldTree f z (Node left x right) =
    let zr = foldTree f z right
    in foldTree f (x `f` zr) left

{-
test :: IO ()
test = do
    let t1 :: BinaryTree Int
        t1 = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)
        t1' = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)
    assertEq "mapTree" t1' $ mapTree (+1) t1

    let t2 :: BinaryTree Int
        t2 = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)
    assertEq "preorder" [2, 1, 3] $ preorder t2
    assertEq "inorder" [1, 2, 3] $ inorder t2
    assertEq "postorder" [1, 3, 2] $ postorder t2

    let t3 :: BinaryTree Int
        t3 = let i = insert' in i 10 $ i 0 $ i 2 $ i 4 $ i 6 Leaf
    assertEq "foldTree-sum" 22 $ foldTree (+) 0 t3
    assertEq "foldTree-list" [0, 2, 4, 6, 10] $ foldTree (:) [] t3

    assertEq "Vigenere"
        [ ('H','a'), ('e','b'), ('l','c'), ('l','a'), ('o','b'), (',','c')
        , (' ',' '), ('w','a'), ('o','b'), ('r','c'), ('l','a'), ('d','b')
        , ('!','c')]
        $ cipherVigenere (KeyWord "abc") (PlainText "Hello, world!")
    -- assertEq "Vigenere" "MPPR AE OYWY" $ cipherVigenere "ALLY" "MEET AT DAWN"
-}
