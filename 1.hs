import Data.Char

getScore n cs = sum $ zipWith scoreOfPair (stringToDigitList cs) (rot n (stringToDigitList cs))

getScore1 cs = getScore 1 cs

getScore2 cs = getScore (fromIntegral (length cs) `div` 2) cs
       
stringToDigitList = map digitToInt 

rot n xs = drop n xs ++ take n xs

scoreOfPair a b | a == b    = a
                | otherwise = 0