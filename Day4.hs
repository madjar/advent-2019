{-# LANGUAGE OverloadedStrings #-}
import Prelude ()
import Protolude
import Unsafe
import Data.Text (splitOn)
import Control.Arrow ((&&&))

main = interact (show . (part1 &&& part2) . parse )

parse :: Text -> (Int, Int)
parse = (\[a, b] -> (a, b)) . map (unsafeFromJust . readMaybe . toS) . splitOn "-"

mkRange (a, b) = [a..b]

isValid = twoAdjacent &&^ neverDecrease

showS :: Show a => a -> [Char]
showS = show

pairs n = let digits = showS n
          in zip digits (unsafeTail digits)

twoAdjacent = any (uncurry (==)) . pairs

neverDecrease = all (uncurry (<=)) . pairs

part1 = length . filter isValid . mkRange

exactlyTwoAdjacent = elem 2 . map length . group . showS

part2 = length . filter (exactlyTwoAdjacent &&^ neverDecrease) . mkRange
