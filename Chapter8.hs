{-# OPTIONS_GHC -Wall -Werror #-}
module Chapter8 where

import Data.List (intersperse)

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
main = f 12324546 -- putStrLn (wordNumber 12324546)
  where
    f n = print n >> putStrLn (wordNumber n)
