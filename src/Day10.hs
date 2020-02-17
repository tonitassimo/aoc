module Day10 where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Control.Monad (guard)
import Numeric.Extra (intToFloat)
import Data.List (sortBy, groupBy)
import Data.Function (on)

input = 
    [
    ".###.###.###.#####.#",
    "#####.##.###..###..#",
    ".#...####.###.######",
    "######.###.####.####",
    "#####..###..########",
    "#.##.###########.#.#",
    "##.###.######..#.#.#",
    ".#.##.###.#.####.###",
    "##..#.#.##.#########",
    "###.#######.###..##.",
    "###.###.##.##..####.",
    ".##.####.##########.",
    "#######.##.###.#####",
    "#####.##..####.#####",
    "##.#.#####.##.#.#..#",
    "###########.#######.",
    "#.##..#####.#####..#",
    "#####..#####.###.###",
    "####.#.############.",
    "####.#.#.##########."
    ]

toSet :: [[Char]] -> S.Set (Int, Int)
toSet input = S.fromList $ do
    (y, row)  <- zip [0..] input
    (x, cell) <- zip [0..] row
    guard     $ cell == '#'
    pure (x, y)

-- Part I

type Angle = (Maybe Float, Sign, Sign)

data Sign = Pos | Neg deriving (Eq, Show, Ord)

sign v = if v < 0 then Neg else Pos

angle :: (Int, Int) -> (Int, Int) -> Angle
angle (x0, y0) (x, y) =
    let
        dx = x - x0
        dy = y - y0
        slope = intToFloat dy / intToFloat dx
    in
        (if dx == 0 then Nothing else Just slope, sign dx, sign dy)

visible :: (Int, Int) -> S.Set (Int, Int) -> Int
visible (x0, y0) possible =
    S.size $
    S.map (angle (x0, y0)) $
    S.delete (x0, y0) possible

asteroids = toSet input

answer = S.lookupMax $ S.map (\point -> (visible point asteroids, point)) asteroids

-- Part II

radius :: (Int, Int) -> (Int, Int) -> Float
radius (x0, y0) (x, y) =
    sqrt (intToFloat (x-x0)**2 + intToFloat (y-y0)**2)

base :: (Int, Int)
base = (8,16)

-- ((point, radius), angle)

vaporizeOrder :: (Int, Int) -> S.Set (Int, Int) -> [(Int, Int)]
vaporizeOrder (x0, y0) possible =
    unwrap $
    map (map (fst . fst)) $
    groupBy ((==) `on` snd) $
    sortBy clockOrder $
    sortBy (compare `on` (snd . fst)) $
    map (\point -> ((point, radius (x0, y0) point), angle (x0, y0) point)) $
    S.toList $
    S.delete (x0, y0) possible

    where
        unwrap [] = []
        unwrap ((r:iR):oR) = r:unwrap (oR ++ [iR])
        unwrap ([]:oR) = unwrap oR

          

clockOrder (_, (s1, Pos, Neg)) (_, (s2, Pos, Neg)) = compare s1 s2
clockOrder (_, (_, Pos, Neg)) _ = LT
clockOrder _ (_, (s1, Pos, Neg)) = GT
clockOrder (_, (s1, Pos, Pos)) (_, (s2, Pos, Pos)) = compare s1 s2
clockOrder (_, (_, Pos, Pos)) _ = LT
clockOrder _ (_, (s1, Pos, Pos)) = GT
clockOrder (_, (s1, Neg, Pos)) (_, (s2, Neg, Pos)) = compare s1 s2
clockOrder (_, (_, Neg, Pos)) _ = LT
clockOrder _ (_, (s1, Neg, Pos)) = GT
clockOrder (_, (s1, Neg, Neg)) (_, (s2, Neg, Neg)) = compare s1 s2

solve = print $ (vaporizeOrder base asteroids) !! 199