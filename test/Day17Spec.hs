module Day17Spec (runDay17) where

import Advent.Day17.Crucible (test, res, townMap, tripScore, initTrip, TownMap, Step(Step), Trip(Trip), moveNext,
    PointTrip(PtTrip), rank, quickLook, prettyPrint, reversePoint)
import Advent.Util.MapUtil (Direction (East, North, South, West), Point(Pt))
import Test.Framework (assertEqual)

runDay17 :: IO ()
runDay17 = do
    quickScoreTest
    quickScoreRes
    reverseTest
    quickLookSteps

testMap:: TownMap
testMap = townMap test

firstRes:: [PointTrip]
firstRes  = [PtTrip (Pt 1 2) (Trip 1 East 1 4) [Step (Pt 1 1) East 0],PtTrip (Pt 2 1) (Trip 1 South 1 3) [Step (Pt 1 1) East 0]]


quickScoreTest:: IO ()
quickScoreTest = do
        assertEqual result 145
        where
            result = tripScore $ quickLook 1000000 (townMap test) initTrip


quickScoreRes:: IO ()
quickScoreRes = do
        assertEqual result 1457
        where
            result = tripScore $ quickLook 1000000 (townMap res) initTrip

initStepTest:: IO ()
initStepTest = do
        print "day 17"
        print testMap
        print $ moveNext testMap initTrip
        assertEqual (moveNext testMap initTrip) firstRes

rankFirstStep:: IO ()
rankFirstStep = do
        print (rank testMap firstRes)
        assertEqual (rank testMap firstRes) (last firstRes)

quickLookSteps:: IO () 
quickLookSteps = do
                    print testMap
                    print $ quickLook 50 testMap initTrip
                    prettyPrint $ quickLook 50 testMap initTrip


reverseTest:: IO () 
reverseTest = do
                    -- print testMap
                    -- print "Four Point"
                    -- prettyPrint fourPoint
                    -- print "Reversed Point"
                    -- print r
                    -- prettyPrint r
                    -- print "Revere Next"
                    -- print n
                    assertEqual n [PtTrip (Pt 1 2) (Trip 3 North 1 9) [Step (Pt 2 2) East 1,Step (Pt 2 1) South 1,Step (Pt 1 1) East 0,Step (Pt 2 3) East 2],PtTrip (Pt 3 2) (Trip 3 South 1 7) [Step (Pt 2 2) East 1,Step (Pt 2 1) South 1,Step (Pt 1 1) East 0,Step (Pt 2 3) East 2]]
                    where
                        fourPoint =  quickLook 4 testMap initTrip
                        r = reversePoint testMap fourPoint
                        n = moveNext testMap r
                     