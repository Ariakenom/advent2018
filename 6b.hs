import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (foldl', sort, minimumBy, sortBy)
import Data.Function (on)
import Data.Char (toLower)
import Debug.Trace

-- error: 41639, used <=10000 instead of <10000

main = interact (
    show
    . solve
    . map (\[y,x] -> (y,x)) . map (map read). map words . lines . filter (/=',')
    )

solve xs =
    let
        (yMin,yMax,xMin,xMax) = bounds xs
        grid = M.fromList [((y,x), sum . map (dist (y,x)) $ xs )| y <- [yMin..yMax], x <- [xMin..xMax]]

    in
        length [()| (_, dists) <- M.assocs grid, dists < 10000]


bounds xs =
    let (y,x) = head xs
    in
        foldr
        (\(y,x) (yMin,yMax,xMin,xMax) -> (min yMin y, max yMax y, min xMin x, max xMax x))
        (y,y,x,x)
        xs


dist (y0,x0) (y1,x1) = abs (y0-y1) + abs (x0-x1)
