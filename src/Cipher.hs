module Cipher where

import Data.Char (chr, isAscii, isPrint, ord)

newtype PlainText = PlainText String
  deriving (Eq, Ord, Show)

newtype KeyWord = KeyWord String
  deriving (Eq, Ord, Show)

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

alphabet :: String
alphabet = filter (\c -> isAscii c && isPrint c) $ map chr [0..127]

encode :: Char -> Char -> Char
encode p k | elem p alphabet && elem k alphabet =
    let base = ord (head alphabet)
        idx c = ord c - base
        delta = (idx p + idx k) `mod` (length alphabet)
    in chr (base + delta)
encode _ _ = error "Only printable ASCII characters are supported"

-- XXX REFACTORME
decode :: Char -> Char -> Char
decode p k | elem p alphabet && elem k alphabet =
    let base = ord (head alphabet)
        idx c = ord c - base
        delta = (idx p - idx k) `mod` (length alphabet)
    in chr (base + delta)
decode _ _ = error "Only printable ASCII characters are supported"
