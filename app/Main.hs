module Main (main1, main) where

import Data.List (sort)

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
  let dimensions = map splitNumbers $ lines contents
  print $ sum $ map requiredArea dimensions
  print $ sum $ map requiredRibbon dimensions

splitNumbers :: String -> [Int]
splitNumbers x =
  let l = takeWhile (not . (flip elem "x")) x
      remainder1 = drop 1 $ dropWhile (not . (flip elem "x")) x
      w = takeWhile (not . (flip elem "x")) remainder1
      h = drop 1 $ dropWhile (not . (flip elem "x")) remainder1
   in map read [l, w, h]

requiredArea :: [Int] -> Int
requiredArea [l,w,h] =
  let areas = [l*w, w*h, h*l]
      smallestArea = minimum areas
  in 2 * sum areas + smallestArea
  
requiredRibbon :: [Int] -> Int
requiredRibbon dimensions = 
  let twoSmallestDimensions = take 2 $ sort dimensions
  in 2 * sum twoSmallestDimensions + product dimensions