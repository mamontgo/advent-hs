module Advent.Util.ListUtil (splitMid, columns, maybeHead, maybeTail, singleton, combos, intoList, variations, takeWhileInclusive, elemIndexes, replaceFirst) where

elemIndexes :: Eq a => a -> [a] -> [Int]
elemIndexes a xs = snd $ foldl (\init_ v -> (fst init_ + 1, if v == a then snd init_ ++ [fst init_] else snd init_)) (0, [] :: [Int]) xs

replaceFirst :: (a -> Bool) -> a -> [a] -> Maybe [a]
replaceFirst = replaceFirstRec []

replaceFirstRec :: [a] -> (a -> Bool) -> a -> [a] -> Maybe [a]
replaceFirstRec init_ f v (h : t) = if f h then Just (init_ ++ (v : t)) else replaceFirstRec (init_ ++ [h]) f v t
replaceFirstRec init_ _ _ [] = Nothing

splitMid :: [a] -> ([a], [a])
splitMid [] = ([], [])
splitMid [a] = ([], [a])
splitMid m = splitAt middle m
  where
    middle = div (length m) 2

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x : xs) =
  x
    : if p x
      then takeWhileInclusive p xs
      else []

intoList :: [a] -> [a] -> a -> [a]
intoList h t i = h ++ [i] ++ t

columns :: (Eq a) => a -> [[a]] -> [[a]]
columns _ [] = []
columns d m
  | any (/= []) m = h : columns d t
  | otherwise = []
  where
    h = maybeHead d <$> m
    t = maybeTail <$> m

maybeTail :: [a] -> [a]
maybeTail [] = []
maybeTail (_ : as) = as

maybeHead :: a -> [a] -> a
maybeHead a [] = a
maybeHead _ (a : _) = a

singleton :: a -> [a]
singleton x = [x]

combos :: Monoid a => [[a]] -> [a]
combos [] = []
combos (h : t) = foldl comboItems h t

comboItems :: Monoid a => [a] -> [a] -> [a]
comboItems a = (mappend <$> a <*>)

variations :: (a -> a) -> [a] -> [[a]]
variations f = variationsBuild f []

variationsBuild :: (a -> a) -> [a] -> [a] -> [[a]]
variationsBuild _ _ [] = []
variationsBuild f init_ (h : t) = (init_ ++ [f h] ++ t) : variationsBuild f (init_ ++ [h]) t
