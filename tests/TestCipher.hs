module TestCipher (testCipher) where

import Data.Char (ord)

import Test.Hspec (describe, hspec, it)
import Test.QuickCheck
  ( Arbitrary(arbitrary)
  , elements
  , listOf
  , listOf1
  , property
  )

import Cipher (SomeText(CipherText,PlainText), caesar, vigenere)

newtype Letter = Letter { unLetter :: Char }
  deriving Show

newtype Keyword = Keyword String
  deriving Show

newtype SomeText' = SomeText' SomeText
  deriving Show

instance Arbitrary Letter where
    arbitrary = Letter <$> elements ['a'..'z']

instance Arbitrary Keyword where
    arbitrary = listOf1 arbitrary >>= pure . Keyword . map unLetter

instance Arbitrary SomeText' where
    arbitrary = do
        letters <- listOf arbitrary
        mkSomeText <- elements [PlainText, CipherText]
        pure . SomeText' . mkSomeText $ map unLetter letters

testCipher :: IO ()
testCipher = hspec $ do
    describe "Ciphers return the same data after encoding and decoding" $ do
        it "Caesar" $ do
            property $ \(SomeText' text) -> check caesar text
        it "Vigenere" $ do
            property $ \(Keyword keyword, SomeText' text) ->
                check (vigenere keyword) text
    it "Caesar shifts by 13" $ do
        property $ \(Letter c) -> case caesar (PlainText [c]) of
            Right (CipherText [c']) -> abs (ord c' - ord c) == 13
            _ -> False
  where
    check :: (SomeText -> Either String SomeText) -> SomeText -> Bool
    check f s = either Left f (f s) == Right s
