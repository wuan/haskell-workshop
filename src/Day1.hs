module Day1 (day1) where

day1 :: IO ()
day1 = do
    contents <- readFile "input-1"
    print "day 1:"
    print $ whichFloor contents
    print $ part2 $ lander contents

--santa [] = 0
--santa ('(':xs) = 1 + santa xs
--santa (')':xs) = -1 + santa xs

santa '(' = 1
santa ')' = -1
santa _ = 0

whichFloor xs = sum (map santa xs)

isPositive x = x >= 0

part2 x = length $ takeWhile isPositive x

lander x = scanl (+) 0 (map santa x)
