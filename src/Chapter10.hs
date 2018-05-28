module Chapter10 where

-- 10.10.1.b
stopVowelStop :: [String]
stopVowelStop = [ [x, y, z]
                | x <- stops, y <- vowels, z <- stops,
                  x == 'p'
                ]
  where
    stops = "pbtdkg"
    vowels = "aeiou"

-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
--
-- > foldr f z []     = z
-- > foldr f z (x:xs) = x `f` foldr f z xs
--
-- foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)

foldrShow :: String
foldrShow = let f x z = concat ["(", x, " `f` ", z, ")"]
                xs = map (('x':) . show) [0..3 :: Int]
            in foldr f "z" xs

-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
--
-- > foldl f z []     = z
-- > foldl f z (x:xs) = let z' = z `f` x
-- >                    in foldl f z' xs
--
-- foldl f z [x1, x2, ..., xn] == (((z `f` x1) `f` x2) `f` x3) ... `f` xn

foldlShow :: String
foldlShow = let f z x = concat ["(", z, " `f` ", x, ")"]
                xs = map (('x':) . show) [0..3 :: Int]
            in foldl f "z" xs

myOr :: Foldable t => t Bool -> Bool
myOr = foldr (||) False

myAny :: Foldable t => (a -> Bool) -> t a -> Bool
myAny f = foldr ((||) . f) False

mySum :: (Num a, Foldable t) => t a -> a
mySum = foldr (+) 0

showFolds :: (String, String)
showFolds = let f x y = concat ["(", x, "+", y, ")"]
                xs = map show ([1..5] :: [Int])
            in (foldl f "0" xs, foldr f "0" xs)

showFoldr :: String
showFoldr = snd showFolds

showFoldl :: String
showFoldl = fst showFolds
