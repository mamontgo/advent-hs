{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Advent.Day14.Dish (Pt, getPts, getPt, promote, display, sortRocks, gridDisplay, stringDisplay, score, tiltNorth, tiltWest, tiltEast, tiltSouth, tiltCycle, detectPattern, getPoint) where
import Advent.Util.FileUtil (load)
import Advent.Util.Funct (cycleCall)
import Advent.Util.ListUtil (columns)
import Data.List (elemIndex)


data Pt = Rock|Cube|Empty deriving (Show, Eq)

score:: [[Pt]] -> Int
score = fst . foldr (\p init_ -> (fst init_ + scoreLine (snd init_) p, (snd init_)+1)) (0, 1)

scoreLine:: Int -> [Pt] -> Int
scoreLine i = (i*) . length . filter (==Rock)

getPoint:: Int -> [[Pt]] -> [[Pt]]
getPoint p x = cycleCall (pos_) tiltCycle x
                where
                    pos_ = ((p - fst pattern_) `mod` (snd pattern_))  + fst pattern_
                    pattern_ = detectPattern 0 [] x

detectPattern:: Int -> [[[Pt]]] -> [[Pt]] -> (Int, Int)
detectPattern index_ init_ current_ = case lookup_ of
                                        Just p -> (p+1, index_ -p)
                                        Nothing -> detectPattern (index_ + 1) (init_ ++ [updated_]) updated_
                                where
                                    lookup_ = elemIndex updated_ init_
                                    updated_ = tiltCycle current_

tiltCycle:: [[Pt]] -> [[Pt]]
tiltCycle  = tiltEast . tiltSouth . tiltWest . tiltNorth

tiltNorth:: [[Pt]] -> [[Pt]]
tiltNorth = promote

tiltSouth:: [[Pt]] -> [[Pt]]
tiltSouth = (columns Empty) . (reverse<$>) . (promoteRocks<$>) . (reverse<$>) . (columns Empty)


tiltWest:: [[Pt]] -> [[Pt]]
tiltWest = (promoteRocks<$>)

tiltEast:: [[Pt]] -> [[Pt]]
tiltEast = (reverse<$>) . (promoteRocks<$>) . (reverse<$>)


promote:: [[Pt]] -> [[Pt]]
promote p = columns Empty $ promoteRocks  <$> columns Empty p

promoteRocks:: [Pt] -> [Pt]
promoteRocks = sortRocks []

sortRocks:: [Pt] -> [Pt] -> [Pt]
sortRocks init_ [] = init_
sortRocks init_ (h:t)
    | h == Rock =  sortRocks ((reverse . insertRock [] . reverse) init_) t
    | otherwise = sortRocks (init_ ++ [h]) t

insertRock:: [Pt] -> [Pt] -> [Pt]
insertRock init_ [] = init_ ++ [Rock]
insertRock init_ (h:t)
    | h /= Empty = init_ ++ [Rock] ++ [h] ++ t
    | otherwise = insertRock (init_ ++ [h]) t

getPts :: String -> IO [[Pt]]
getPts s =   fmap getPt <$> rows
            where rows = load s

getPt:: String -> [Pt]
getPt = (asPt <$>)

asPt:: Char -> Pt
asPt c
    | c == 'O' = Rock
    | c == '#' = Cube
    | otherwise = Empty

gridDisplay:: [[Pt]] -> IO ()
gridDisplay pts = do
                    mconcat $ fmap print chrs
                    where
                        chrs = (asCh <$>) <$> pts

display:: [[Pt]] -> [String]
display pts = (asCh <$>) <$> pts

stringDisplay:: [Pt] -> String
stringDisplay = (asCh <$>)

asCh:: Pt -> Char
asCh Rock = 'O'
asCh Cube = '#'
asCh Empty = '.'




