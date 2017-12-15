module Chapter10 where

showFolds :: (String, String)
showFolds = let f x y = concat ["(", x, "+", y, ")"]
                xs = map show ([1..5] :: [Int])
            in (foldl f "0" xs, foldr f "0" xs)

showFoldr :: String
showFoldr = snd showFolds

showFoldl :: String
showFoldl = fst showFolds
