module Day2 where

import Data.List (sort)

day2 :: IO ()
day2 = do
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
requiredArea [l, w, h] =
  let areas = [l * w, w * h, h * l]
      smallestArea = minimum areas
   in 2 * sum areas + smallestArea

requiredRibbon :: [Int] -> Int
requiredRibbon dimensions =
  let twoSmallestDimensions = take 2 $ sort dimensions
   in 2 * sum twoSmallestDimensions + product dimensions
