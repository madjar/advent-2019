{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

import Prelude ()
import Protolude
import Unsafe
import Data.Text (splitOn)
import Control.Arrow ((&&&))
import Data.Array.ST

main = interact (show . (part1 &&& part2) . parse )

parse :: Text -> [Int]
parse = map (unsafeFromJust . readMaybe . toS) . splitOn ","

part1 input = runProgramWithArgs input 12 2

runProgramWithArgs :: [Int] -> Int -> Int -> Int
runProgramWithArgs program noun verb = runST $ do
  array <- newListArray (0, length program) program
  writeArray array 1 noun
  writeArray array 2 verb
  run array 0
  readArray array 0

run :: STUArray s Int Int -> Int -> ST s ()
run array pos = do
  opcode <- readArray array pos
  case opcode of
    99 -> return ()
    1 -> runOpCode (+)
    2 -> runOpCode (*)

  where runOpCode f = do
          posA <- readArray array (pos + 1)
          posB <- readArray array (pos + 2)
          a <- readArray array posA
          b <- readArray array posB
          writePos <- readArray array (pos + 3)
          writeArray array writePos (f a b)
          run array (pos + 4)

part2 program = do
  noun <- [0..99]
  verb <- [0..99]
  let output = runProgramWithArgs program noun verb
  guard (output == 19690720)
  return (100 * noun + verb)
