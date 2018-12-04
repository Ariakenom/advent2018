import qualified Data.Map as M
import Data.List (foldl', sort, tails)
import Control.Monad (replicateM)
import Data.Maybe (mapMaybe)

-- wrong srijafjzloguvlvtqmphenbkd
-- off by 1 on take drop

main = interact (
    show
    . solve
    . lines
    )

solve = head . mapMaybe (uncurry diffBy1) . combinations

diffBy1 a b =
    let
        diffs = filter snd . zip [0..] $ zipWith (/=) a b
    in case diffs of
        [(i,_)] -> Just (take i a ++ drop (i+1) a)
        _ -> Nothing

combinations :: [a] -> [(a,a)]
combinations xs = [(x, y) | x : ys <- tails xs, y <- ys]
