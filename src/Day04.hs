module Day04
    ( findValidPasswords
    ) where

import Control.Monad (guard)
import Data.List (group)

-- Types and constants

type Password = [Int]

lowerBound :: Int
lowerBound = 134564

higherBound :: Int
higherBound = 585159

range :: [Int]
range = [lowerBound..higherBound]

-- Conversion

intToPassword :: Int -> Password
intToPassword 0 = []
intToPassword i = intToPassword (i `div` 10) ++ [i `mod` 10]

possiblePasswords :: [Int] -> [Password]
possiblePasswords = map intToPassword  

-- Logic

findValidPasswords :: [Password] -> [Password]
findValidPasswords pws = filter isPasswordValid pws

isPasswordValid :: Password -> Bool
isPasswordValid pw = (isLengthValid pw) && (containsAdjacentNumbers pw) && (consistsOfGrowingNumbers pw)

isLengthValid :: Password -> Bool
isLengthValid pw = (length pw) <= 6

containsAdjacentNumbers :: Password -> Bool
containsAdjacentNumbers [] = False
containsAdjacentNumbers (x:[]) = False
containsAdjacentNumbers (first:second:tail) = if first == second
                                                then True
                                                else containsAdjacentNumbers (second:tail)

consistsOfGrowingNumbers :: Password -> Bool
consistsOfGrowingNumbers [] = True
consistsOfGrowingNumbers (x:[]) = True
consistsOfGrowingNumbers (first:second:xs) = if first > second
                                                then False
                                                else consistsOfGrowingNumbers (second:xs)



--- Refactoring and part II

s = 134564
e = 585159

exTwoAdj = elem 2 . map length . group . show

notDecr = go . show
    where
        go (x:y:rest) = x <= y && go (y:rest)
        go [x] = True
        go _ = False

possible = do
    v <- [s..e]
    guard $ exTwoAdj v && notDecr v
    pure v

solve = print $ length possible