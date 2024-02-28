module Advent.Day15.LavaHash (encode, encodeAll, hashScore, asOp, test, ops, boxScore) where

import Advent.Util.ListUtil (replaceFirst)
import Data.Char (ord)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import Text.Read as R

data Op = AddOp String Int | DelOp String deriving (Show, Eq)

data Lense = Lense {label :: String, count :: Int} deriving (Show, Eq)

data Box = Box [Lense] deriving (Show, Eq)

buildBoxes :: [Op] -> V.Vector Box
buildBoxes = foldl updateBox boxes

updateBox :: V.Vector Box -> Op -> V.Vector Box
updateBox v o = V.update v (V.singleton (index, updatedBox))
  where
    updatedBox = performBoxOp currentBox o
    currentBox = v V.! index
    index = encode (opLabel o)

boxes :: V.Vector Box
boxes = V.replicate 256 (Box [])

calcTotalBoxValue :: V.Vector Box -> Int
calcTotalBoxValue v = sum $ (\i -> (i+1) * calcBoxValue (v V.! i)) <$> [0 .. 255]

calcBoxValue :: Box -> Int
calcBoxValue (Box xs) = snd $ foldl (\(i, t) v -> (i + 1, t + (i * (count v)))) (1, 0) xs

test :: String
test = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

boxScore:: String -> IO Int
boxScore = (( calcTotalBoxValue . buildBoxes . ops) <$>) . readFile

hashScore :: String -> IO Int
hashScore = (sum <$>) . (encodeAll <$>) . readFile

encodeAll :: String -> [Int]
encodeAll = (encode <$>) . splitOn ","

encode :: String -> Int
encode = foldl (\init_ v -> (v + init_) * 17 `mod` 256) 0 . (ord <$>)

opLabel :: Op -> String
opLabel (DelOp l) = l
opLabel (AddOp l _) = l

ops :: String -> [Op]
ops = (asOp <$>) . splitOn ","

asOp :: String -> Op
asOp s
  | '=' `elem` s = asAddOp (splitOn "=" s)
  | otherwise = asDelOp (splitOn "-" s)

asAddOp :: [String] -> Op
asAddOp [a, b] = AddOp a (R.read b)
asAddOp _ = AddOp "" 0

asDelOp :: [String] -> Op
asDelOp (h : _) = DelOp h
asDelOp _ = DelOp ""

performBoxOp :: Box -> Op -> Box
performBoxOp (Box xs) o@(DelOp l) = Box (filter ((/= l) . label) xs)
performBoxOp (Box []) o@(AddOp l v) = Box [Lense l v]
performBoxOp (Box xs) o@(AddOp l v) = Box $ fromMaybe (xs ++ [n]) m
  where
    m = replaceFirst ((== l) . label) n xs
    n = Lense l v
