module Advent.Day13.Mirror(Points, get, printp, printpc, getPoints, matchAll, score, scoreAll) where

import Advent.Util.FileUtil(load)
import Data.List.Split 
import Advent.Util.ListUtil (columns)
import Data.Maybe (fromMaybe)

data Point = Rock|Ash deriving (Show, Eq)
type Points = [Point]

getPoints :: String -> IO [[Points]]
getPoints s = (asPointsArray <$>) <$> get s

get:: String -> IO [[String]]
get s = splitWhen (=="") <$> load s

asPointsArray:: [String] -> [Points]
asPointsArray = fmap asPoints

asPoints:: String -> Points
asPoints = fmap asPoint

asPoint:: Char -> Point
asPoint c = if c == '#' then Rock else Ash

checkAllReflection:: [Points] -> (Bool, Int)
checkAllReflection p = checkAllReflectionPoints (matchAll p) p

checkAllReflectionPoints:: [Int] -> [Points] -> (Bool, Int)
checkAllReflectionPoints [] _ = (False, 0)
checkAllReflectionPoints (h:t) a 
    | not (fst m) && t /= [] = checkAllReflectionPoints t a
    | otherwise = m  
    where 
        m = checkMatchedReflection h a


checkMatchedReflection:: Int -> [Points] -> (Bool, Int)
checkMatchedReflection _ [] = (False, 0)
checkMatchedReflection p a = (checkReflection $ splitAt (p+1) a, p+1)


checkReflection:: ([Points], [Points]) -> Bool
checkReflection = checkReflect . reflectMatch . reverseReflect 


scoreAll:: [[Points]] -> Int
scoreAll p = sum (score <$> p)

score::[Points] -> Int
score p =  fromMaybe (fromMaybe 0 vcalc) hcalc
            where 
                hcalc = (hscore . checkAllReflection) p
                cols = columns Ash p
                vcalc = (vscore . checkAllReflection) cols


hscore:: (Bool, Int) -> Maybe Int
hscore (False, _) = Nothing
hscore (True, s) = Just $ s * 100

vscore:: (Bool, Int) -> Maybe Int
vscore (False, _) = Nothing
vscore (True, s) = Just s 

checkReflect:: Maybe ([Points], [Points]) -> Bool
checkReflect Nothing = False
checkReflect (Just ([], _)) = True
checkReflect (Just (_, [])) = True
checkReflect (Just (a:as, b:bs))  
    | a == b = checkReflect (Just (as, bs))
    | otherwise = False

reverseReflect:: ([Points], [Points]) -> ([Points], [Points])
reverseReflect (a, b) = (reverse a, b)

reflectMatch :: ([Points], [Points]) -> Maybe ([Points], [Points])
reflectMatch([], _) = Nothing
reflectMatch(_, []) = Nothing
reflectMatch p@(a : as, b : bs)
  | a == b = Just p
  | hasBtail && b == head bs = Just (b:a:as, bs)
  | hasAtail && head as == a = Just (as, a:b:bs)
  | otherwise = Nothing
    where
        hasBtail = not (null bs)
        hasAtail = not (null as) 




matchAll :: [Points] -> [Int]
matchAll [] = []
matchAll (h:t) =  matchAllReflection 0 [] h t


matchAllReflection:: Int -> [Int] -> Points -> [Points] -> [Int]
matchAllReflection _ r _ [] = r
matchAllReflection row c p (h:t) =  if p == h then
                                        matchAllReflection (row+1) (row:c) h t                                    
                                    else
                                        matchAllReflection (row+1) c h t                                    


asChar:: Point -> Char
asChar Rock = '#' 
asChar Ash = '.' 

printpc:: [Points] ->  IO () 
printpc p = printp (columns Ash p)

printp:: [Points] ->  IO () 
printp p =  do 
                mapM_ print x
            where
                x = (asChar <$>) <$> p                    