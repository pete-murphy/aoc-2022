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

run :: IO ()
run = do
  input <- readFile "../input/day06"
  -- let s1 = "bvwbjplbgvbhsrlpgdmjqwftvncz"
  --     s2 = "nppdvjthqldpwncqszvftbrmjlhg"
  --     s3 = "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
  --     s4 = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
  putStrLn "part 1"
  print (part1 input)

  -- print (part1 s1)
  -- print (part1 s2)
  -- print (part1 s3)
  -- print (part1 s4)

  putStrLn "part 2"
  -- putStrLn (part2 input)
