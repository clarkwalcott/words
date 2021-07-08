module Lib
    ( formatGrid
    , printGrid
    , findWord
    , findWords
    , findWordInLine
    , skew
    ) where

import Data.List (isInfixOf, transpose)
import Data.Maybe (catMaybes)

type Grid = [String]

formatGrid :: Grid -> String
formatGrid = unlines

printGrid :: Grid -> IO ()
printGrid = putStrLn . formatGrid

getLines :: Grid -> [String]
getLines grid = 
    let horizontal = grid
        vertical = transpose grid
        diagonal1 = diagonalize grid
        diagonal2 = diagonalize (map reverse horizontal)
        lines = horizontal ++ vertical ++ diagonal1 ++ diagonal2
    in lines ++ (map reverse lines) 

skew :: Grid -> Grid
skew [] = []
skew (l:ls) = l : skew (map indent ls)
  where indent line = '_' : line

diagonalize :: Grid -> Grid
diagonalize = transpose . skew

findWord :: Grid -> String -> Maybe String
findWord grid word = 
    let lines = getLines grid
        found = or $ map (findWordInLine word) lines
    in if found then Just word else Nothing

findWordInLine :: String -> String -> Bool
findWordInLine = isInfixOf

findWords :: Grid -> [String] -> [String]
findWords grid words = 
    let foundWords = map (findWord grid) words
    in catMaybes foundWords
