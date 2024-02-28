module Day15Spec (runDay15) where
import Advent.Day15.LavaHash (hashScore, boxScore)
import Test.Framework (assertEqual)


runDay15:: IO ()
runDay15 = do
            part1TestFileSpec
            part1ResFileSpec
            part2TestFileSpec
            part2ResFileSpec


part2TestFileSpec:: IO ()
part2TestFileSpec = do
                        score <- boxScore "./data/day15/test.txt"
                        print ("Part2 Test Score " ++ show score) 
                        assertEqual score 145


part2ResFileSpec:: IO ()
part2ResFileSpec = do
                        score <- boxScore "./data/day15/res.txt"
                        print ("Part2 Res Score " ++ show score) 
                        assertEqual score 244199


part1TestFileSpec:: IO ()
part1TestFileSpec = do
                        score <- hashScore "./data/day15/test.txt"
                        print ("Part1 Test Score " ++ show score) 
                        assertEqual score 1320

part1ResFileSpec:: IO ()
part1ResFileSpec = do
                        score <- hashScore "./data/day15/res.txt"
                        print ("Part1 RES Score " ++ show score) 
                        assertEqual score 514281                        