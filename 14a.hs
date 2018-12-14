{-# LANGUAGE BangPatterns #-}

import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Data.Maybe (fromJust)
import Data.Char (toLower)
import Control.DeepSeq (force)
import Debug.Trace

-- wrong 1671381310
-- wrong 2621300000
-- thought input was starting recipies ...


main = interact (
    concatMap show
    . solve
    . readInt
    )

digSum = map readInt . map (:[]) . show . F.sum

readInt = (read :: String -> Int)

-- nRec n pos rec
--     | n <= length rec = rec
--     | otherwise =
--         let
--             newRec = rec Seq.>< digSum (map (\p -> (rec!p)) pos)
--             newPos = map (\p -> (1 + p + (rec!p)) `mod` (length newRec)) pos
--         in nRec n newPos newRec

infRec :: [Int] -> Seq.Seq Int -> [Int]
infRec pos rec = F.toList rec ++ go pos rec
    where
        go pos !rec =
            let
                diffRec = digSum (map (\p -> (rec!p)) pos)
                newRec = rec Seq.>< Seq.fromList diffRec
                !newPos = force $ map (\p -> (1 + p + (rec!p)) `mod` (length newRec)) pos
            in diffRec ++ go newPos newRec

solve :: Int -> [Int]
solve n =
    let
        pos = [0,1]
        ys = infRec pos (Seq.fromList [3,7])
        zs = take 10 $ drop n ys
    in
        zs

(!) xs i = fromJust (Seq.lookup i xs)
