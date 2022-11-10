module Main (main) where

main1 = do
  contents <- readFile "input-1"
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

-- day 2 starts here

main = do
  contents <- readFile "input-2"
  print $ sum $ map (presentCalculation . presentNumbers) (lines contents)

presentNumbers :: String -> [Int]
presentNumbers x =
  let l = takeWhile (not . (flip elem "x")) x
      remainder1 = drop 1 $ dropWhile (not . (flip elem "x")) x
      w = takeWhile (not . (flip elem "x")) remainder1
      h = drop 1 $ dropWhile (not . (flip elem "x")) remainder1
   in map read [l, w, h]

presentCalculation :: [Int] -> Int
presentCalculation numbers@[l,w,h] = 
  let smallest = minimum numbers
  in
   2*l*w + 2*w*h + 2*h*l + smallest