module Day06 where

import Control.Arrow ((>>>))
import qualified Data.List as List

part1 :: String -> Int
part1 =
  List.tails
    >>> map (take 4)
    >>> zip [4 ..]
    >>> dropWhile (\(_, xs) -> List.nub xs /= xs)
    >>> head
    >>> fst

part2 :: String -> Int
part2 =
  List.tails
    >>> map (take 14)
    >>> zip [14 ..]
    >>> dropWhile (\(_, xs) -> List.nub xs /= xs)
    >>> head
    >>> fst

run :: IO ()
run = do
  input <- readFile "../input/day06"
  putStrLn "part 1"
  print (part1 input)
  putStrLn "part 2"
  print (part2 input)
