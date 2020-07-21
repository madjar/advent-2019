import Prelude ()
import Protolude
import Unsafe
import Data.Text (lines)
import Control.Arrow ((&&&))

main = interact (show . (part1 &&& part2) . parse )

parse :: Text -> [Int]
parse = map (unsafeFromJust . readMaybe . toS) . lines

part1 :: [Int] -> Int
part1 = sum . map computeFuel

computeFuel :: Int -> Int
computeFuel mass = mass `div` 3 - 2

part2 :: [Int] -> Int
part2 = sum . map (computeFuelOfFuel . computeFuel)

computeFuelOfFuel :: Int -> Int
computeFuelOfFuel = sum . takeWhile (> 0 ) . iterate computeFuel
