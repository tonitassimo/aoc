module Day02 where

import qualified Data.Map.Strict as M

-- Creation and transformation of input data

input :: [Int]
input = [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,19,1,19,6,23,2,23,13,27,1,27,5,31,2,31,10,35,1,9,35,39,1,39,9,43,2,9,43,47,1,5,47,51,2,13,51,55,1,55,9,59,2,6,59,63,1,63,5,67,1,10,67,71,1,71,10,75,2,75,13,79,2,79,13,83,1,5,83,87,1,87,6,91,2,91,13,95,1,5,95,99,1,99,2,103,1,103,6,0,99,2,14,0,0]

inputWithIndex :: [Int] -> [(Int, Int)]
inputWithIndex list = zip [0..] list 

inputToMap :: [(Int, Int)] -> M.Map Int Int
inputToMap tuples = M.fromList tuples

convert :: [Int] -> M.Map Int Int
convert = inputToMap . inputWithIndex

initial :: M.Map Int Int
initial = M.insert 2 2 $ M.insert 1 12 $ convert input

-- Logic

executeProgram :: M.Map Int Int -> Int -> Maybe (M.Map Int Int)
executeProgram program index = do
    opCode <- M.lookup index program
    case opCode of
        99 -> pure program
        n -> do
            leftPos <- M.lookup (index + 1) program
            leftVal <- M.lookup leftPos program
            rightPos <- M.lookup (index + 2) program
            rightVal <- M.lookup rightPos program
            resultPos <- M.lookup (index + 3) program
            op <- getOp n
            executeProgram (M.insert resultPos (op leftVal rightVal) program) (index + 4)

getOp :: Int -> Maybe (Int -> Int -> Int)
getOp 1 = Just (+)
getOp 2 = Just (*)
getOp _ = Nothing

-- Part II

goal :: Maybe Int
goal = Just 19690720

calculateResult :: Int -> Int -> Int
calculateResult noun verb = 100 * noun + verb

initial' :: Int -> Int -> M.Map Int Int
initial' noun verb = M.insert 2 verb $ M.insert 1 noun $ convert input

possiblePrograms :: [(Int, Int, M.Map Int Int)]
possiblePrograms = [0..99] >>=
    ( \noun -> [0..99] >>=
    ( \verb -> pure $ (noun, verb, initial' noun verb)))

-- Main

solve2 =
    case filter (\(_, _, t) -> (executeProgram t 0 >>= M.lookup 0) == goal) possiblePrograms of
        ((noun, verb, _):_) -> print $ calculateResult noun verb
        _ -> print "No result found"

solve =
    print $ executeProgram initial 0 >>= M.lookup 0
