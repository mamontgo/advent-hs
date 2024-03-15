module Advent.Util.Funct ( 
    (<$*>), 
    cycleCall, 
    ioCall, 
    cycleBreakingCall, 
    cycleBreakingCallIO, 
    ioCallWithMessage, 
    cycleCallCollect,
    combine,
    (&&&)
) where


(<$*>) :: Functor f => f (a -> b) -> a -> f b
x <$*> y = fmap (\m -> m y) x


ioCall:: (a -> a) -> IO a -> IO a
ioCall f ioa = do f <$> ioa


(&&&):: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) = combine (&&)

combine:: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
combine f x y a = f (x a) (y a)

ioCallWithMessage:: Show a => String -> (a -> a) -> IO a -> IO a
ioCallWithMessage s f ioa = do
                a <- ioa
                print s
                print a
                return (f a)

cycleCallCollect:: Int -> (a -> a) -> a -> [a]
cycleCallCollect 0 r _ = []
cycleCallCollect n f r = res:cycleCallCollect (n-1) f res
                        where res = f r

cycleCall:: Int -> (a -> a) -> a -> a
cycleCall 0 _ r = r
cycleCall n f r = cycleCall (n-1) f $! f r

cycleBreakingCall:: Eq a => Int -> (a -> a) -> a -> (Int, a)
cycleBreakingCall 0 _ r = (-1, r)
cycleBreakingCall n f r
    | fres == r =  (n, r)
    | otherwise = cycleBreakingCall count f $! fres
                    where
                        fres = f r
                        count = n-1

cycleBreakingCallIO:: (Eq a, Show a)  => Int -> (a -> a) -> a -> IO (Int, a)
cycleBreakingCallIO 0 _ r = do return (-1, r)
cycleBreakingCallIO n f r = do
                                print ("Matching " ++ show isMatch)
                                print r
                                print fres
                                print (r==fres)
                                if isMatch then
                                    return (n, r)
                                else
                                    cycleBreakingCallIO count f $! fres
                                where
                                    isMatch = fres == r
                                    fres = f r
                                    count = n-1
