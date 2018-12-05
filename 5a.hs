import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (foldl', sort)
import Data.Char (toLower)
import Debug.Trace

-- wrong 70 wrong input
-- wrong 10322 lost a character when go repeated

main = interact (
    show
    . length . shrink
    . head . lines
    )

shrink xs = go False [] xs
    where
        go again xs (x:y:ys)
            | x /= y && toLower x == toLower y = go True xs ys
            | otherwise = go again (x:xs) (y:ys)
        go False xs ys = (reverse xs)++ys
        go True xs ys  = go False [] ((reverse xs)++ys)
