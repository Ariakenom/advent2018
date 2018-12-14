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

digSum p0 p1 = map readInt . map (:[]) . show $ p0 + p1

readInt = (read :: String -> Int)


infRec :: [Int] -> Seq.Seq Int -> [Int]
infRec [p0,p1] rec = F.toList rec ++ go p0 p1 rec
    where
        go p0 p1 rec =
            let
                diffRec = digSum (rec `Seq.index` p0) (rec `Seq.index` p1)
                newRec = foldl' (Seq.|>) rec diffRec
                p0' = (1 + p0 + (rec `Seq.index` p0)) `mod` (length newRec)
                p1' = (1 + p1 + (rec `Seq.index` p1)) `mod` (length newRec)
            in diffRec ++ go p0' p1' newRec

solve :: [Int] -> Int
solve xs =
    let
        pos = [0,1]
        ys = infRec pos (Seq.fromList [3,7])
        n = head [i | (i, as) <- zip [0..] (tails ys), isPrefixOf xs as]
    in
        n

(!) xs i = fromJust (Seq.lookup i xs)
