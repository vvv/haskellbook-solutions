module Chapter16
  ( e_ch16
  ) where

e_ch16 :: IO Integer
e_ch16 = let ioi = readIO "1" :: IO Integer
             changed = read . ("123" ++) . show <$> ioi
         in (*3) <$> changed
