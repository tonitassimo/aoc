module Day22 where

import qualified Data.Map.Strict as M
import Data.List (elemIndex)

input :: [(Int, Int)]
input = [
    (2 , 21),
    (0, 0),
    (2 , 52),
    (0, 0),
    (2 , 19),
    (0, 0),
    (2 , 65),
    (1 , -8368),
    (0, 0),
    (1 , -3820),
    (2 , 47),
    (1 , -2965),
    (2 , 38),
    (0, 0),
    (1 , 9165),
    (2 , 62),
    (1 , 3224),
    (2 , 72),
    (1 , 4120),
    (2 , 40),
    (1 , -9475),
    (2 , 52),
    (1 , 4437),
    (0, 0),
    (1 , 512),
    (2 , 33),
    (1 , -9510),
    (0, 0),
    (1 , -6874),
    (2 , 56),
    (1 , -47),
    (2 , 7),
    (0, 0),
    (2 , 13),
    (1 , 4530),
    (2 , 67),
    (0, 0),
    (1 , 8584),
    (2 , 26),
    (1 , 4809),
    (2 , 72),
    (1 , -8003),
    (2 , 24),
    (1 , 1384),
    (0, 0),
    (2 , 13),
    (0, 0),
    (1 , -1171),
    (2 , 72),
    (1 , 6614),
    (2 , 61),
    (1 , 1412),
    (2 , 16),
    (1 , -4808),
    (0, 0),
    (2 , 51),
    (1 , -8507),
    (2 , 60),
    (1 , 1202),
    (2 , 2),
    (1 , -4030),
    (2 , 4),
    (1 , -9935),
    (2 , 57),
    (1 , -6717),
    (2 , 5),
    (0, 0),
    (1 , 3912),
    (2 , 64),
    (1 , 5152),
    (0, 0),
    (2 , 68),
    (0, 0),
    (1 , 49),
    (2 , 31),
    (1 , 4942),
    (2 , 44),
    (1 , -4444),
    (2 , 47),
    (1 , -5533),
    (2 , 51),
    (1 , -3045),
    (2 , 67),
    (1 , -1711),
    (2 , 46),
    (1 , -6247),
    (0, 0),
    (1 , 969),
    (2 , 55),
    (1 , 7549),
    (2 , 62),
    (1 , -9083),
    (2 , 54),
    (0, 0),
    (1 , -3337),
    (2 , 62),
    (1 , 1777),
    (2 , 39),
    (1 , 3207),
    (2 , 13)
    ]

deck :: [Int]
deck = [0..10006]

smallDeck :: [Int]
smallDeck = [0..9]

newStack :: [Int] -> [Int]
newStack = reverse

cut :: Int -> [a] -> [a]
cut n cards
    | n > 0 =
        let cutted = take n cards in
            reverse ((reverse cutted) ++ (reverse (drop n cards)))
    | otherwise =
        let nAbs = abs n
            cutted = take nAbs (reverse cards) in            
            (reverse cutted) ++ (reverse (drop nAbs (reverse cards)))


-- left deck = map from initial list
-- right deck = initially empty map
--
-- lookup value in left deck at source index (source index is incremented by one every round)
-- put value in right deck at destination index (destination index is incremented by the value of 'n' every round)
--                                               -> if n > maximal count of items => n = n mod maximal count of items
-- repeat until left deck is empty
-- by then the right deck should contain all cards 
-- in a shuffled "order"
dealWithIncrement :: Int -> [Int] -> [Int]
dealWithIncrement n cards =
    let leftDeck = M.fromList (zip [0..] cards)
        rightDeck = M.empty
        maxCount = length cards in
        dealWithIncrement' 0 0 n leftDeck rightDeck maxCount
    where
        dealWithIncrement' src dst step leftDeck rightDeck maxCount
            | not $ M.null leftDeck = dealWithIncrement' (src + 1) (mod (dst + step) maxCount) step (M.delete src leftDeck) (M.insert dst
                (case M.lookup src leftDeck of
                    Just x -> x
                    Nothing -> error "error occured")
                rightDeck) maxCount
            | otherwise = M.elems rightDeck

smallDeckToMap :: M.Map Int Int
smallDeckToMap = M.fromList (zip [0..] smallDeck)

test =
    cut (-1) $
    dealWithIncrement 3 $
    dealWithIncrement 9 $
    cut 3 $
    dealWithIncrement 7 $
    cut (-4) $
    cut 8 $
    dealWithIncrement 7 $
    cut (-2) $
    newStack smallDeck

solve :: [(Int, Int)] -> [Int] -> [Int]
solve [] cards = cards
solve ((x, y):instructions) cards =
    let 
        op = case x of
            0 -> newStack
            1 -> cut y
            2 -> dealWithIncrement y
    in
        solve instructions (op cards)

