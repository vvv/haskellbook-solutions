module Cipher
  ( SomeText(CipherText,PlainText)
  , caesar
  , vigenere
  ) where

-- XXX TODO: Support upper-case letters.

import Data.Char (ord)
import Data.Either (partitionEithers)

data SomeText
  = PlainText String
  | CipherText String
  deriving (Eq, Show)

alphabet :: String
alphabet = ['a'..'z']

idx :: Char -> Int
idx c = ord c - ord (head alphabet)

encode :: Char -> Char -> Char
encode k a = cycle alphabet !! (idx a + idx k)

decode :: Char -> Char -> Char
decode k b = cycle alphabet !! (idx b + length alphabet - idx k)

tryTo :: (Char -> Char -> Char) -> (Char, Char) -> Either String Char
tryTo f (k, a)
  | k `notElem` alphabet = mkErr "keyword" k
  | a `notElem` alphabet = mkErr "plaintext" a
  | otherwise = Right (f k a)
  where
    mkErr src c = Left $ "Unsupported character in " ++ src ++ ": " ++ show c

vigenere :: String -> SomeText -> Either String SomeText
vigenere "" _ = Left "Empty keyword"
vigenere keyword text =
    let (xcode, txt, wrap) = case text of
            PlainText s -> (encode, s, CipherText)
            CipherText s -> (decode, s, PlainText)
    in case partitionEithers $ map (tryTo xcode) $ zip (cycle keyword) txt of
        ([], txt')   -> Right (wrap txt')
        ((err:_), _) -> Left err

caesar :: SomeText -> Either String SomeText
caesar = let k = alphabet !! (length alphabet `div` 2)
         in vigenere [k]
