import qualified Data.Map as M
import Data.List (foldl', sort, tails)
import Control.Monad (replicateM)
import Data.Maybe (mapMaybe)

main = interact (
    show
    . solve
    . map (map readInt) . map words . lines . blankOut "#@,:x"
    )

blankOut chars = map (\c -> if c `elem` chars then ' ' else c)

readInt :: String -> Integer
readInt = read

solve =
    fst . head . filter snd . M.assocs
    . foldr insertIds M.empty . M.elems
    . foldr (\(i,y,x) -> M.insertWith (++) (y,x) [i]) M.empty
    . concatMap (\[i,x,y,w,h] -> [ (i, y',x') | y' <- [y..(y+h-1)], x' <- [x..(x+w-1)]])

insertIds :: [Integer] -> M.Map Integer Bool -> M.Map Integer Bool
insertIds [i] m = M.insertWith (&&) i True m
insertIds is m = foldr (\i -> M.insertWith (&&) i False) m is
