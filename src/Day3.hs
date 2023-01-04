module Day3 where

import Data.List


day3 :: IO ()
day3 = do
  contents <- readFile "input-3"
  let coordinates = visited contents
  print $ length $ nub coordinates

  let (santa, robo) = uninterleave contents
  let coordinates' = visited santa <> visited robo
  print $ length $ nub coordinates'


-- new name for Int
type Number = Int

-- wraps an existing type
newtype MyNumber = MyNumber Int

type Coordinate = (Int, Int)

visited :: String -> [] Coordinate
visited input = scanl (flip step) (0, 0) input

--step :: Char -> Coordinate -> Either[Coordinate, Excption]
step :: Char -> Coordinate -> Coordinate
step '<' (x, y) = (x -1, y)
step '>' (x, y) = (x + 1, y)
step 'v' (x, y) = (x, y -1)
step '^' (x, y) = (x, y + 1)
step x y = error ("step got bad input " <> show x <> " " <> show y)

-- taken from DSP.Basic
uninterleave :: [a] -> ([a],[a])
-- tilde is an irrefutible pattern match
uninterleave = foldr (\x ~(xs,ys) -> (x:ys,xs)) ([],[])