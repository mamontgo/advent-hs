module Scratch where
import Advent.Util.ListUtil 

x1 = [["1", "2"]]
y1 = [["3", "4"], ["5", "6"]]
a1 =  ext <$> x1
-- print a1 <*> y1


extList:: [[[a]]]

let a = [["1", "2"]]
let b = y1 = [["3", "4"], ["5", "6"]]

let f = (flip ext) (head b)
fmap (\m -> f m) a


f = ioCallWithMessage "Result:" tiltCycle
join $ gridDisplay <$> (cycleCall 100 f)  (getPts "../data/day14/test.txt")


join $  (gridDisplay) <$> (cycleCall 20 tiltCycle) <$> (getPts "../data/day14/test.txt")

join $ (gridDisplay) <$> (getPoint 20)  <$> (getPts "../data/day14/test.txt")


detectPattern 0 []  <$> (getPts "../data/day14/test.txt")
