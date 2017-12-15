module Chapter07 where

tensDigit :: Integral a => a -> a
tensDigit x = x `div` 10 `mod` 10

hunsD :: Integral a => a -> a
hunsD x = x `div` 100 `mod` 10
