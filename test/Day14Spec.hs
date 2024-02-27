module Day14Spec (runDay14) where
import Advent.Day14.Dish (Pt, sortRocks, getPoint, tiltCycle, display, getPt, getPts, stringDisplay, promote, score)
import Advent.Util.Funct (cycleCall)
import Test.Framework

runDay14:: IO ()
runDay14 = do
        sortRocksSpec
        promoteSpec
        scoreSpec
        scoreResSpec
        getPointSpec
        getPatternTestPointScore
        getPatternResPointScore



getPatternTestPointScore:: IO () 
getPatternTestPointScore = do
                        points <- pointsio
                        print ("PART2: Test Score " ++ show points)
                        assertEqual points 64
                        where
                                pointsio =  score . getPoint 1000000000 <$> getPts "./data/day14/test.txt"

getPatternResPointScore:: IO () 
getPatternResPointScore = do
                        points <- pointsio
                        print ("PART2: Res Score " ++ show points)
                        assertEqual points 102943
                        where
                                pointsio =  score . getPoint 1000000000 <$> getPts "./data/day14/res.txt"

getPointSpec:: IO ()
getPointSpec = do
                res <- points
                assertEqual (and res) True
                where
                        points =  (<$>[25..100]) . compareGet <$> getPts "./data/day14/test.txt"

scoreSpec:: IO ()
scoreSpec = do
                x <- iox
                print $ "Test score: " ++ show x
                assertEqual x 136
                where iox = score . promote <$> getPts "./data/day14/test.txt"


scoreResSpec:: IO ()
scoreResSpec = do
                x <- iox
                print $ "Res score: " ++ show x
                assertEqual x 105208
                where iox = score . promote <$> getPts "./data/day14/res.txt"

promoteSpec:: IO ()
promoteSpec = do
                res <- iores
                print res
                assertEqual res ["OOOO.#.O..","OO..#....#","OO..O##..O","O..#.OO...","........#.","..#....#.#","..O..#.O.O","..O.......","#....###..","#....#...."]
                where iores = display . promote <$> getPts "./data/day14/test.txt"


sortRocksSpec:: IO ()
sortRocksSpec = do
        print (stringDisplay x)
        assertEqual (stringDisplay x) "#O..#O.."
        where x = sortRocks [] (getPt "#..O#..O")

compareGet:: [[Pt]] -> Int -> Bool
compareGet init_ pos = cycle_ == pattern_
        where
                cycle_ = cycleCall pos tiltCycle init_
                pattern_ = getPoint pos init_


