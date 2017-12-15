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
    assertEq "Vigenere"
        [ ('H','a'), ('e','b'), ('l','c'), ('l','a'), ('o','b'), (',','c')
        , (' ',' '), ('w','a'), ('o','b'), ('r','c'), ('l','a'), ('d','b')
        , ('!','c')]
        $ cipherVigenere (KeyWord "abc") (PlainText "Hello, world!")
    -- assertEq "Vigenere" "MPPR AE OYWY" $ cipherVigenere "ALLY" "MEET AT DAWN"
-}
