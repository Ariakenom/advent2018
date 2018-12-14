import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (foldl', sort)
{-# LANGUAGE BangPatterns #-}

import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Data.Maybe (fromJust)
import Data.Char (toLower)
import Debug.Trace

-- wrong 1671381310
-- wrong 2621300000
-- thought input was starting recipies ...


main = interact (
    concatMap show
    . solve
    . readInt
    )

digSum = Seq.fromList . map readInt . map (:[]) . show . F.sum

readInt = (read :: String -> Int)

nRec n pos rec
    | n <= length rec = rec
    | otherwise =
        let
            newRec = rec Seq.>< digSum (map (\p -> (rec!p)) pos)
            newPos = map (\p -> (1 + p + (rec!p)) `mod` (length newRec)) pos
        in nRec n newPos newRec

solve :: Int -> [Int]
solve n =
    let
        pos = [0,1]
        ys = nRec (n+10) pos (Seq.fromList [3,7])
        zs = Seq.take 10 $ Seq.drop n ys
    in
        F.toList zs

xs ! i = fromJust (Seq.lookup i xs)
