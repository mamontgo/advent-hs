module ListUtilSpec (runListUtil) where

import Advent.Util.ListUtil (combos)
import Test.Framework

runListUtil:: IO () 
runListUtil = do 
        extCombosSpec

extCombosSpec:: IO ()
extCombosSpec = do
                print (combos [["1","2"],["3","4"],["5","6"]])
                assertEqual (combos [["1","2"],["3","4"],["5","6"]]) ["135","136","145","146","235","236","245","246"]
