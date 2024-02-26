module Advent.Day13.Mirror (Points, Point(Rock, Ash), get, printp, printpc, getPoints, matchAll, score, scoreAll, scoreAllCombo, toggle,checkAllReflection, checkAdjustedReflections) where

import Advent.Util.FileUtil(load)
import Data.List.Split
import Advent.Util.ListUtil (columns, intoList)
import Data.Maybe (fromMaybe)

data Point = Rock|Ash deriving (Show, Eq)
data ReflectionResult = ReflectionResult {valid :: Bool, start:: Int}
type Points = [Point]

type ReflectionResultFunction = [Points] -> ReflectionResult

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

reflectFailed:: ReflectionResult
reflectFailed = ReflectionResult False 0


checkAdjustedReflections:: [Points] -> ReflectionResult
checkAdjustedReflections [] =  reflectFailed
checkAdjustedReflections p = checkAdjustedReflectionsMap [] p

checkAdjustedReflectionsMap:: [Points] -> [Points] -> ReflectionResult
checkAdjustedReflectionsMap _ [] = reflectFailed
checkAdjustedReflectionsMap init_ (h:t) = if valid res then res else checkAdjustedReflectionsMap (init_ ++ [h]) t
                                        where
                                            res = foldl (\ _ x -> x) reflectFailed ll
                                            ll = takeWhile valid results
                                            results = checkAllReflection <$> toggleCombos
                                            toggleCombos = (\x -> init_ ++ [x] ++ t) <$> togglePointCombos
                                            togglePointCombos = togglePoints h

checkAllReflection:: [Points] -> ReflectionResult
checkAllReflection p = checkAllReflectionPoints (matchAll p) p

checkAllReflectionPoints:: [Int] -> [Points] -> ReflectionResult
checkAllReflectionPoints [] _ = reflectFailed
checkAllReflectionPoints (h:t) a
    | not (valid m) && t /= [] = checkAllReflectionPoints t a
    | otherwise = m
    where
        m = checkMatchedReflection h a


checkMatchedReflection:: Int -> [Points] -> ReflectionResult
checkMatchedReflection _ [] = reflectFailed
checkMatchedReflection p a = ReflectionResult (checkReflection $ splitAt (p+1) a) (p+1)


checkReflection:: ([Points], [Points]) -> Bool
checkReflection = checkReflect . reflectMatch . reverseReflect

scoreAllCombo:: [[Points]] -> Int
scoreAllCombo = sum . (score checkAdjustedReflections <$>)


scoreAll:: [[Points]] -> Int
scoreAll = sum . (score checkAllReflection <$>)

score:: ReflectionResultFunction -> [Points] -> Int
score f p =  fromMaybe (fromMaybe 0 vcalc) hcalc
            where
                hcalc = (hscore . f) p
                cols = columns Ash p
                vcalc = (vscore . f) cols


hscore:: ReflectionResult -> Maybe Int
hscore (ReflectionResult False _) = Nothing
hscore (ReflectionResult True s) = Just $ s * 100

vscore:: ReflectionResult -> Maybe Int
vscore (ReflectionResult False _) = Nothing
vscore (ReflectionResult True s) = Just s

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



togglePoints:: Points -> [Points]
togglePoints [] = []
togglePoints h = togglePointCollect [] [] h

togglePointCollect :: [Points] -> Points -> Points -> [Points]
togglePointCollect res _ [] = res
togglePointCollect res init_ (h:t) =  togglePointCollect (res ++ [init_ ++ [togglePoint h] ++ t]) (init_++[h]) t



togglePoint:: Point -> Point
togglePoint Ash = Rock
togglePoint Rock = Ash


matchAll :: [Points] -> [Int]
matchAll [] = []
matchAll (h:t) =  matchAllReflection 0 [] h t


matchAllReflection:: Int -> [Int] -> Points -> [Points] -> [Int]
matchAllReflection _ r _ [] = r
matchAllReflection row c p (h:t) =  if p == h then
                                        matchAllReflection (row+1) (row:c) h t
                                    else
                                        matchAllReflection (row+1) c h t






-- Printing util


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