import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (foldl', sort, minimumBy, sortBy)
import Data.Function (on)
import Data.Char (toLower)
import Debug.Trace

-- error: confused node and coordinate when computing areas

main = interact (
    show
    . solve
    . map (\[y,x] -> (y,x)) . map (map read). map words . lines . filter (/=',')
    )

solve xs =
    let
        (yMin,yMax,xMin,xMax) = bounds xs
        grid = M.fromList [((y,x), findClosest xs (y,x))| y <- [yMin..yMax], x <- [xMin..xMax]]
        edges =
            nubOrd
            $  [(y,x)| y <- [yMin..yMax], x <- [xMin, xMax]]
            ++ [(y,x)| y <- [yMin, yMax], x <- [xMin..xMax]]
        infNodes = S.fromList [grid M.! yx| yx <- edges]
        candidateNodes = S.fromList xs `S.difference` (infNodes `S.union` S.fromList [noClosest])
        area = M.fromListWith (+) [(node, 1)| (_, node) <- M.assocs grid]
    in
        maximum . map (area M.!) $ S.toList candidateNodes


bounds xs =
    let (y,x) = head xs
    in
        foldr
        (\(y,x) (yMin,yMax,xMin,xMax) -> (min yMin y, max yMax y, min xMin x, max xMax x))
        (y,y,x,x)
        xs

findClosest xs yx =
    let
        distyx = dist yx
        byCloseness = sortBy (compare `on` distyx) xs
    in case byCloseness of
        (a:b:_) | distyx a == distyx b -> noClosest
        (a:_) -> a

noClosest = (-1,-1)

dist (y0,x0) (y1,x1) = abs (y0-y1) + abs (x0-x1)

nubOrd = S.toList . S.fromList
