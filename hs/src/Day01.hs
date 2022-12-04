module Day01 where

import Control.Arrow
import Data.Function
import Data.Functor
import Data.List
import Data.Maybe (fromMaybe)
import Data.Ord (Down (Down, getDown))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Text.Read (readMaybe)

part1 :: Text -> Int
part1 input =
  input
    & Text.splitOn "\n\n"
    <&> Text.lines
    <&> (map (Text.unpack >>> readMaybe >>> fromMaybe 0) >>> sum)
    & maximum

part2 :: Text -> Int
part2 input =
  input
    & Text.splitOn "\n\n"
    <&> Text.lines
    <&> (map (Text.unpack >>> readMaybe >>> fromMaybe 0) >>> sum)
    <&> Down
    & sort
    & take 3
    <&> getDown
    & sum

run :: IO ()
run = do
  input <- Text.readFile "../input/day01"
  putStrLn "part 1"
  print (part1 input)
  putStrLn "part 2"
  print (part2 input)
