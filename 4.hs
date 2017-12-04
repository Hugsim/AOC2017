import Data.List

main1 = do
    input <- readFile "input4.hs"
    return (length (filter allDifferent (map words $ lines input)))

allDifferent []     = True
allDifferent (x:xs) = x `notElem` xs && allDifferent xs

main2 = do 
    input <- readFile "input4.hs"
    return (length (filter allNotAnagrams (map words $ lines input)))

allNotAnagrams []     = True
allNotAnagrams (x:xs) = all (notAnagram x) xs && allNotAnagrams xs

notAnagram x y = not (sort x == sort y)