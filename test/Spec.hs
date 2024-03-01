import Day13Spec (runDay13)
import Day14Spec (runDay14)
import Day15Spec (runDay15)
import Day16Spec (runDay16)
import ListUtilSpec (runListUtil)

main :: IO ()
main = do

                printTestRes
                runListUtil
                runDay13
                runDay14 
                runDay15 
                runDay16



printTestRes:: IO() 
printTestRes = do
                    print "Welcome To Tests"