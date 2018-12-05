import qualified Data.Map as M
import Data.List (foldl', sort, tails)
import Control.Monad (replicateM)
import Data.Maybe (mapMaybe)

main = interact (
    show
    . solve
    . map (map read). map words . lines . blankOut "#@,:x"
    )

blankOut chars = map (\c -> if c `elem` chars then ' ' else c)

solve =
    length . filter (2<=) . M.elems
    . foldr (\yx -> M.insertWith (+) yx 1) M.empty
    . concatMap (\[_,x,y,w,h] -> [ (y',x') | y' <- [y..(y+h-1)], x' <- [x..(x+w-1)]])


