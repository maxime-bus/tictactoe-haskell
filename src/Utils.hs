module Utils where

init' :: [a] -> [a]
init' [] = []
init' list = init list

-- See http://stackoverflow.com/questions/26040420/in-haskell-what-is-the-most-common-way-to-apply-a-function-to-every-nth-element
mapEvery :: Int -> (a -> a) -> [a] -> [a]
mapEvery n f = zipWith ($) (drop 1 . cycle . take n $ f : repeat id)

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = y1 : chunk n y2
  where
    (y1, y2) = splitAt n xs
