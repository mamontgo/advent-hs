module Advent.Util.MapUtil (
    Direction(North, South, East, West),
    Point(Pt),
    invertDir,
    north,
    south,
    east,
    west,
    navigate,
    directions,
    nextDirections
) where

data Direction = North | South | East | West deriving (Show, Eq, Ord)
data Point = Pt Int Int deriving (Show, Eq, Ord)


-- if the approaching direction is East, we don't need to include West as the next direction
-- as that is the point we just come from
nextDirections:: Direction -> [Direction]
nextDirections d = filter (/=invertDir d) directions

directions:: [Direction]
directions = [West, North, East, South]

invertDir:: Direction -> Direction
invertDir North = South
invertDir South = North
invertDir East = West
invertDir West = East

navigate:: Direction -> Point -> Point
navigate North = north
navigate South = south
navigate West = west
navigate East = east

north :: Point -> Point
north (Pt x y) = Pt (x - 1) y

south :: Point -> Point
south (Pt x y) = Pt (x + 1) y

east :: Point -> Point
east (Pt x y) = Pt x (y + 1)

west :: Point -> Point
west (Pt x y) = Pt x (y - 1)