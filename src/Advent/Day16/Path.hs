module Advent.Day16.Path (readPt, pointsTraveled, points, test) where

import Advent.Util.FileUtil (load)
import Advent.Util.ListUtil (singleton)
import Advent.Util.Funct ((<$*>))
import qualified Data.Set as S
import qualified Data.Matrix as M

-- map point types
data PointType = SplitNS| SplitEW | MirrorB | MirrorF | Empty deriving (Show, Eq)

-- a point in the matrix
data Point = Pt Int Int deriving (Show, Eq, Ord)

-- movement direction in the matrix
data Direction = North|South|East|West deriving (Show, Eq, Ord)

-- visit a point heading in a direction
data Visit = Visit Direction Point deriving (Show, Eq, Ord)

-- a set of points that have been visited heading in a particular direction
type Visited = S.Set Visit

-- Journey Map  (Just a matrix of space types)
type JourneyMap =  M.Matrix PointType
data Journey = Journey JourneyMap Visited deriving (Show)

initVisit:: Visit
initVisit = Visit East $ Pt 1 1

initJourney:: JourneyMap -> Journey
initJourney m = Journey m S.empty

getVisited:: Journey -> Visited
getVisited (Journey _ v) = v

getPoint:: Visit -> Point
getPoint (Visit _ p) = p 

pointsTraveled:: JourneyMap -> Int
pointsTraveled m = S.size $ S.fromList $ getPoint <$> S.toList (getVisited $ doJourney [initVisit] (initJourney m)) 

-- main journey loop
doJourney:: [Visit] -> Journey -> Journey
doJourney [] j = j
doJourney (h@(Visit _ p):t) j@(Journey m v) =   doJourney (newVisits ++ t) (Journey m newVisited)
                                            where
                                                pointType = getPointType p m
                                                newVisits = filterVisits j $ nextVisitPoints h pointType
                                                newVisited = S.insert h $ v `S.union` S.fromList newVisits

filterVisits:: Journey -> [Visit] -> [Visit]
filterVisits (Journey m s) = filter (and . ([not . hasVisited s,validPointRange m]<$*>))

getPointType:: Point -> JourneyMap -> PointType
getPointType (Pt x y) = M.getElem x y


-- calculate next point(s) based on the visit of a point type
nextVisitPoints:: Visit -> PointType -> [Visit]
nextVisitPoints (Visit d p) pt = nextPoints pt d p

nextPoints:: PointType -> Direction -> Point  -> [Visit]
nextPoints MirrorF East  = singleton . north
nextPoints MirrorB East  = singleton . south
nextPoints SplitNS East  = ([north, south]<$*>)
nextPoints _ East = singleton . east
nextPoints MirrorF West  = singleton . south
nextPoints MirrorB West  = singleton . north
nextPoints SplitNS West  = ([north, south]<$*>)
nextPoints _ West = singleton . west
nextPoints MirrorF South  = singleton . west
nextPoints MirrorB South  = singleton . east
nextPoints SplitEW South  = ([east, west]<$*>)
nextPoints _ South  = singleton . south
nextPoints MirrorF North  = singleton . east
nextPoints MirrorB North  = singleton . west
nextPoints SplitEW North  = ([east, west]<$*>)
nextPoints _ North  = singleton . north


north:: Point -> Visit
north (Pt x y) = Visit North $ Pt (x-1) y

south:: Point -> Visit
south (Pt x y) = Visit South $ Pt (x+1) y

east:: Point -> Visit
east (Pt x y) = Visit East $ Pt x (y+1)

west:: Point -> Visit
west (Pt x y) = Visit West $ Pt x (y-1)


-- has a visit already been visited 
hasVisited:: Visited -> Visit ->  Bool
hasVisited = flip S.member

-- is a new point within a valid range of the map
validPointRange:: JourneyMap -> Visit -> Bool
validPointRange m (Visit _ (Pt x y)) = (x > 0) && x <= M.nrows m && (y > 0) && y <= M.ncols m

points:: [String] -> JourneyMap
points xs = M.fromLists $ (point <$>) <$> xs

readPt:: String -> IO [String]
readPt = load

point:: Char -> PointType
point '|' = SplitNS
point '-' = SplitEW
point '/' = MirrorF
point '\\' = MirrorB
point _ = Empty


chr:: PointType -> Char
chr SplitNS = '|'
chr SplitEW = '-'
chr MirrorF = '/'
chr MirrorB = '\\'
chr _ = '.'


test:: [String]
test = [".|...\\....","|.-.\\.....",".....|-...","........|.","..........",".........\\","..../.\\\\..",".-.-/..|..",".|....-|.\\","..//.|...."]
