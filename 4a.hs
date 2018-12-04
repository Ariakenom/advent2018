import qualified Data.Map as M
import Data.List (foldl', sort)
import Debug.Trace
{-
[1518-03-18 23:50] Guard #2591 begins shift
[1518-10-30 00:48] falls asleep
[1518-07-14 00:40] wakes up
-}

{-
Wrong: 23776 too high
-}

data Event =
    Shift Integer
    | Wake Integer
    | Sleep Integer

main = interact (
    show
    . findMax . sched M.empty
    . map parseLine
    . sort . lines
    )

parseLine :: String -> Event
parseLine = (
    fun
    . words . filter (/=']') . filter (/='#')
    . drop 15 -- drop until minutes
    )

fun (min:one:two:_) = case one of
        "Guard" -> Shift (read two)
        "falls" -> Sleep (read min)
        "wakes" -> Wake (read min)

sched m (Shift guard:xs) = schedShift guard m 0 xs
sched m [] = m

schedShift _ m _ [] = sched m []
schedShift guard m prev xs@(Shift _:_) = sched m xs
schedShift guard m prev (Sleep now:xs) = schedShift guard m now xs
schedShift guard m prev (Wake now:xs) =
    let m2 = foldl' (\m1 i -> M.insertWith (+) (guard,i) 1 m1) m [prev..(now-1)]
    in  schedShift guard m2 now xs

findMax :: M.Map (Integer,Integer) Integer -> Integer
findMax guardmin2nr =
    let
        guard2nr = M.foldrWithKey' (\(guard,_) nr -> M.insertWith (+) guard nr) M.empty guardmin2nr
        (_,lazyGuard) = M.foldrWithKey' (\guard nr -> max (nr,guard)) (-1,-1) guard2nr
        min2nr =
            M.mapKeys (\(guard,min) -> min)
            . M.filterWithKey (\(guard,min) _ -> lazyGuard == guard)
            $ guardmin2nr
        (_,lazyMin) = M.foldrWithKey' (\min nr -> max (nr,min)) (-1,-1) min2nr
    in lazyGuard*lazyMin
