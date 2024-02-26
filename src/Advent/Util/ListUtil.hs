module Advent.Util.ListUtil (splitMid, columns, maybeHead, maybeTail, singleton, combos, inList) where

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

inList:: [a] -> [a] -> a -> [a]
inList h t i = h ++[i]++t

maybeTail::  [a] -> [a]
maybeTail [] = []
maybeTail (_:as) = as

maybeHead:: a -> [a] -> a
maybeHead a [] = a
maybeHead _ (a:_) = a

singleton :: a -> [a]
singleton x = [x]

combos:: Monoid a => [[a]] -> [a]
combos [] = []
combos (h:t) = foldl comboItems h t

comboItems:: Monoid a => [a] -> [a] -> [a]
comboItems a = (mappend <$> a <*>)


-- extCombos:: [[[a]]] -> [[[a]]]
-- extCombos [] = []
-- extCombos (h:t) = extListCombos [h] t

-- -- [["1","2"],["3","4"],["5","6"]]
-- -- [["135","136"],["145","146"],["235","236"],["245","246"]]

-- extListCombos:: [[[a]]] -> [[[a]]] -> [[[a]]]
-- extListCombos init_ [] = init_
-- extListCombos init_ (h:t) = extListCombos res t
--                     where
--                         f = flip extArrayCombos h
--                         res = concatMap f init_

-- -- let a = ["5", "6"]
-- -- let b = ["1", "2"]
-- -- ext a b
-- -- [["51","52"],["61","62"]]
-- extArrayCombos:: [[a]] -> [[a]] -> [[[a]]]
-- extArrayCombos a b = fmap (\x -> fmap (x ++) b) a
