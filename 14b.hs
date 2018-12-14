{-# LANGUAGE BangPatterns #-}

import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Data.Maybe (fromJust)
import Data.Char (toLower)
import Data.List (foldl', sort, isPrefixOf, tails)
import Debug.Trace

main = interact (
    show
    . solve
    . map readInt . map (:[]) . head . words
    )

digSum = Seq.fromList . map readInt . map (:[]) . show . F.sum

readInt = (read :: String -> Int)

infRec :: [Int] -> Seq.Seq Int -> [Int]
infRec pos rec = F.toList rec ++ go pos rec
    where
        go pos rec =
            let
                diffRec = digSum (map (\p -> (rec!p)) pos)
                newRec = rec Seq.>< diffRec
                newPos = map (\p -> (1 + p + (rec!p)) `mod` (length newRec)) pos
            in F.toList diffRec ++ go newPos newRec

solve :: [Int] -> Int
solve xs =
    let
        pos = [0,1]
        ys = infRec pos (Seq.fromList [3,7])
        n = head [i | (i, as) <- zip [0..] (tails ys), isPrefixOf xs as]
    in
        n

(!) xs i = fromJust (Seq.lookup i xs)
