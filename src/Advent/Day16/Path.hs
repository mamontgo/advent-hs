module Advent.Day16.Path (readPt, pointsTraveled, points, test, maxVisitPoints) where

import Advent.Util.FileUtil (load)
import Advent.Util.MapUtil (Direction(North, South, East, West), Point(Pt), north, south, east, west)
import Advent.Util.Funct ((<$*>))
import Advent.Util.ListUtil (singleton)
import qualified Data.Matrix as M
import qualified Data.Set as S

-- map point types
data PointType = SplitNS | SplitEW | MirrorB | MirrorF | Empty deriving (Show, Eq)

-- visit a point heading in a direction
data Visit = Visit Direction Point deriving (Show, Eq, Ord)

-- a set of points that have been visited heading in a particular direction
type Visited = S.Set Visit

-- Journey Map  (Just a matrix of space types)
type JourneyMap = M.Matrix PointType

data Journey = Journey JourneyMap Visited deriving (Show)

allVisit :: JourneyMap -> [Visit]
allVisit m =
  (Visit South . Pt 1 <$> [1 .. (M.ncols m)])
    ++ (Visit North . Pt (M.nrows m) <$> [1 .. (M.ncols m)])
    ++ (Visit East . (`Pt` 1) <$> [1 .. (M.nrows m)])
    ++ (Visit West . (`Pt` M.ncols m) <$> [1 .. (M.nrows m)])

initVisit:: Visit
initVisit = Visit East (Pt 1 1)

initJourney :: JourneyMap -> Journey
initJourney m = Journey m S.empty

getVisited :: Journey -> Visited
getVisited (Journey _ v) = v

getPoint :: Visit -> Point
getPoint (Visit _ p) = p

maxVisitPoints:: JourneyMap -> Int
maxVisitPoints m = foldl max 0 (snd . visitTravel m <$> allVisit m)

pointsTraveled :: JourneyMap -> Int
pointsTraveled = visitPointsTraveled initVisit

visitTravel:: JourneyMap -> Visit -> (Visit, Int)
visitTravel m v = (v, visitPointsTraveled v m)

visitPointsTraveled :: Visit -> JourneyMap -> Int
visitPointsTraveled v m = S.size $ S.fromList $ getPoint <$> S.toList (getVisited $ doJourney [v] (initJourney m))

-- main journey loop
doJourney :: [Visit] -> Journey -> Journey
doJourney [] j = j
doJourney (h@(Visit _ p) : t) j@(Journey m v) = doJourney (newVisits ++ t) (Journey m newVisited)
  where
    pointType = getPointType p m
    newVisits = filterVisits j $ nextVisitPoints h pointType
    newVisited = S.insert h $ v `S.union` S.fromList newVisits

filterVisits :: Journey -> [Visit] -> [Visit]
filterVisits (Journey m s) = filter (and . ([not . hasVisited s, validPointRange m] <$*>))

getPointType :: Point -> JourneyMap -> PointType
getPointType (Pt x y) = M.getElem x y

-- calculate next point(s) based on the visit of a point type
nextVisitPoints :: Visit -> PointType -> [Visit]
nextVisitPoints (Visit d p) pt = nextPoints pt d p

nextPoints :: PointType -> Direction -> Point -> [Visit]
nextPoints MirrorF East = singleton . visitNorth
nextPoints MirrorB East = singleton . visitSouth
nextPoints SplitNS East = ([visitNorth, visitSouth] <$*>)
nextPoints _ East = singleton . visitEast
nextPoints MirrorF West = singleton . visitSouth
nextPoints MirrorB West = singleton . visitNorth
nextPoints SplitNS West = ([visitNorth, visitSouth] <$*>)
nextPoints _ West = singleton . visitWest
nextPoints MirrorF South = singleton . visitWest
nextPoints MirrorB South = singleton . visitEast
nextPoints SplitEW South = ([visitEast, visitWest] <$*>)
nextPoints _ South = singleton . visitSouth
nextPoints MirrorF North = singleton . visitEast
nextPoints MirrorB North = singleton . visitWest
nextPoints SplitEW North = ([visitEast, visitWest] <$*>)
nextPoints _ North = singleton . visitNorth

visitNorth :: Point -> Visit
visitNorth = Visit North . north

visitSouth :: Point -> Visit
visitSouth = Visit South . south

visitEast :: Point -> Visit
visitEast = Visit East . east

visitWest :: Point -> Visit
visitWest = Visit West . west

-- has a visit already been visited
hasVisited :: Visited -> Visit -> Bool
hasVisited = flip S.member

-- is a new point within a valid range of the map
validPointRange :: JourneyMap -> Visit -> Bool
validPointRange m (Visit _ (Pt x y)) = (x > 0) && x <= M.nrows m && (y > 0) && y <= M.ncols m

points :: [String] -> JourneyMap
points xs = M.fromLists $ (point <$>) <$> xs

readPt :: String -> IO [String]
readPt = load

point :: Char -> PointType
point '|' = SplitNS
point '-' = SplitEW
point '/' = MirrorF
point '\\' = MirrorB
point _ = Empty

chr :: PointType -> Char
chr SplitNS = '|'
chr SplitEW = '-'
chr MirrorF = '/'
chr MirrorB = '\\'
chr _ = '.'

test :: [String]
test = [".|...\\....", "|.-.\\.....", ".....|-...", "........|.", "..........", ".........\\", "..../.\\\\..", ".-.-/..|..", ".|....-|.\\", "..//.|...."]
