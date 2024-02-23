module Advent.Util.ListUtil(splitMid, columns, maybeHead, maybeTail) where


splitMid:: [a] -> ([a], [a])
splitMid [] = ([], [])
splitMid [a] = ([], [a])
splitMid m = splitAt middle m
            where middle = div (length m) 2

columns:: (Eq a) => a -> [[a]] -> [[a]]
columns _ [] = []
columns d m
    | any (/=[]) m = h : columns d t
    | otherwise = []
    where
        h = maybeHead d <$> m
        t = maybeTail <$> m

maybeTail::  [a] -> [a]
maybeTail [] = []
maybeTail (_:as) = as

maybeHead:: a -> [a] -> a
maybeHead a [] = a
maybeHead _ (a:_) = a