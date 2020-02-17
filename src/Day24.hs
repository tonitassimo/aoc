module Day24 where

import qualified Data.Map.Strict as M

input =
    [
        "####.",
        ".###.",
        ".#..#",
        "##.##",
        "###.."
    ]

data Field = Bug | Empty deriving (Show, Eq)

convert :: [[Char]] -> [[Field]]
convert = (map . map) convertChar

convertChar :: Char -> Field
convertChar c =
    case c of
        '#' -> Bug
        '.' -> Empty
        otherwise -> error "unrecognized character"

typedInput = convert input

toMap :: [[Field]] -> M.Map (Int, Int) Field
toMap fields =
    M.fromList $
    do
        (y, row)  <- zip [0..] fields
        (x, cell) <- zip [0..] row
        return ((x, y), cell)

adjacentBugs :: (Int, Int) -> M.Map (Int, Int) Field -> Int
adjacentBugs (x, y) grid =
    let neighbours = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)] in
        sum $ map maybeFieldToInt $ lookupMultipleKeys grid neighbours

lookupMultipleKeys :: Ord k => M.Map k v -> [k] -> [Maybe v]
lookupMultipleKeys map [] = []
lookupMultipleKeys map (k:ks) = M.lookup k map : lookupMultipleKeys map ks

maybeFieldToInt :: Maybe Field -> Int
maybeFieldToInt mF =
    case mF of        
        Just Bug   -> 1
        otherwise  -> 0

answer = print "hi"