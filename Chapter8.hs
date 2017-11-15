{-# OPTIONS_GHC -Wall -Werror #-}
module Chapter8 where

import Data.List (intersperse)
import Debug.Trace (trace)

mc91 :: (Num a, Ord a, Show a) => a -> a
mc91 n | trace ("mc91 " ++ show n) False = undefined
       | n > 100 = n - 10
       | otherwise = mc91 . mc91 $ n + 11

digitToWord :: Int -> String
digitToWord d = case d of
    0 -> "zero" -- "शून्य"
    1 -> "one" -- "एक"
    2 -> "two" -- "दो"
    3 -> "three" -- "तीन"
    4 -> "four" -- "चार"
    5 -> "five" -- "पांच"
    6 -> "six" -- "छह"
    7 -> "seven" -- "सात"
    8 -> "eight" -- "आठ"
    9 -> "nine" -- "नौ"
    x -> '#' : show x

digits :: Int -> [Int]
digits = go []
  where
    go ds n = let (n', d) = divMod n 10
                  ds' = d:ds
              in if n' == 0 then ds' else go ds' n'

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits

main :: IO ()
main = do
    f 12324546
    f 100500
    f 0
    f 1
  where
    f n = putStr "> " >> print n >> putStrLn (wordNumber n)
