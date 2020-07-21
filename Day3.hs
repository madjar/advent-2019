{-# LANGUAGE OverloadedStrings #-}

import Prelude (error)
import Protolude
import Unsafe
--import Data.Text (splitOn)
import Control.Arrow ((&&&))
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import qualified Data.Set as Set
import qualified Data.Map as Map

main = interact (show . (part1 &&& part2) . parseIt parser)

parseIt parser = either (error . errorBundlePretty) identity . parse parser "input"

type Wire = [(Char, Int)]

parser :: Parsec Void Text (Wire, Wire)
parser = (,) <$> wire <* newline <*> wire <* newline <* eof
  where wire = ((,) <$> asciiChar <*> decimal) `sepBy1` char ','

part1 (first, second) = minimum . map manhattan . Set.toList . Set.delete (0, 0) $ (setPositions first `Set.intersection` setPositions second)

setPositions = Set.fromList . positions

positions :: Wire -> [(Int, Int)]
positions wire = go (0, 0) wire
  where go _ [] = []
        go pos ((direction, distance):ws) =
          let step = case direction of
                'R' -> (1, 0)
                'L' -> (-1, 0)
                'U' -> (0, 1)
                'D' -> (0, -1)
              apply (f, g) (a, b) = (f + a, g + b)
              newPositions = unsafeTail $ take (distance + 1) (iterate (apply step) pos)
          in newPositions ++ go (unsafeLast newPositions) ws

manhattan (x, y) = abs x + abs y

part2 (first, second) = let mkLengthMap path = Map.fromListWith min (zip (positions path) [1..])
                            firstLength = mkLengthMap first
                            secondLength = mkLengthMap second
                        in minimum . Map.elems $ Map.intersectionWith (+) firstLength secondLength
