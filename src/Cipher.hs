{-# OPTIONS_GHC -Wall -Werror #-}

module Cipher
  ( KeyWord(..)
  , PlainText(..)
  , alphabet
  , caesar
  , cipherVigenere
  , decode
  , encode
  , uncaesar
  ) where

import Data.Char (chr, isUpper, ord, toLower, toUpper)

newtype PlainText = PlainText String
  deriving (Eq, Ord, Show)

newtype KeyWord = KeyWord String
  deriving (Eq, Ord, Show)

alphabet :: String
alphabet = ['a'..'z']

errorBadChar :: a
errorBadChar = error "Unsupported character"

mkCipher :: (Int -> Char -> Char) -> Int -> Char -> Char
mkCipher f shift c | isUpper c = toUpper . f shift $ toLower c
                   | otherwise = f shift c

caesar :: Int -> Char -> Char
caesar = mkCipher (caesar' False)

uncaesar :: Int -> Char -> Char
uncaesar = mkCipher (caesar' True)

caesar' :: Bool -> Int -> Char -> Char
caesar' _ _ c | c `notElem` alphabet = errorBadChar
caesar' decode_p shift c =
    let len = length alphabet
        shift' = shift `mod` len
        (c', c'') = if decode_p
                    then (ord c - shift', c' + len)
                    else (ord c + shift', c' - len)
    in chr $ if c' < ord (head alphabet) || c' > ord (last alphabet)
             then c''
             else c'

cipherVigenere :: KeyWord -> PlainText
               -> String -- ^ Ciphertext.
cipherVigenere (KeyWord "") _ = error "Empty keyword"
cipherVigenere (KeyWord key) _
  | elem ' ' key = error "Keyword must not contain spaces"
cipherVigenere key plain = go key plain
  where
    go _ (PlainText "") = []
    go (KeyWord "") ps = go key ps
    go ks (PlainText (' ':ps)) = ' ' : go ks (PlainText ps)
    go (KeyWord (k:ks)) (PlainText (p:ps)) =
        encode p k : go (KeyWord ks) (PlainText ps)

encode :: Char -> Char -> Char
encode p k | elem p alphabet && elem k alphabet =
    let base = ord (head alphabet)
        idx c = ord c - base
        delta = (idx p + idx k) `mod` (length alphabet)
    in chr (base + delta)
encode _ _ = errorBadChar

-- XXX REFACTORME
decode :: Char -> Char -> Char
decode p k | elem p alphabet && elem k alphabet =
    let base = ord (head alphabet)
        idx c = ord c - base
        delta = (idx p - idx k) `mod` (length alphabet)
    in chr (base + delta)
decode _ _ = errorBadChar
