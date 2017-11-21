{-# OPTIONS_GHC -Wall -Werror #-}
module Chapter9 where

eftBool :: Bool -> Bool -> [Bool]
eftBool True False = []
eftBool False True = [False, True]
eftBool p _ = [p]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = mkEnumFrom

eftInt :: Int -> Int -> [Int]
eftInt = mkEnumFrom

eftChar :: Char -> Char -> [Char]
eftChar = mkEnumFrom

mkEnumFrom :: (Bounded a, Enum a, Ord a) => a -> a -> [a]
mkEnumFrom a b = go a
  where
    go x | x <= b = x : if x == maxBound
                        then []
                        else go (succ x)
         | otherwise = []

wordsBy :: (a -> Bool) -> [a] -> [[a]]
wordsBy p = go
  where
    go [] = []
    go xs = let word = takeWhile (not . p) xs
                rest = go . dropWhile p . drop (length word) $ xs
            in if null word then rest else word:rest

myWords :: String -> [String]
myWords = wordsBy (== ' ')

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

myReverse :: [a] -> [a]
myReverse = go []
  where
    go acc [] = acc
    go acc (x:xs) = go (x:acc) xs
-- myReverse = foldl' (flip (:)) []
