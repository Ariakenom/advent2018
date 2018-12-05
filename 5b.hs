import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (foldl', sort)
import Data.Char (toLower)
import Debug.Trace

-- wrong 0 filtered to only one char instead of all but

main = interact (
    show
    . manyShrink
    . head . lines
    )

manyShrink xs =
    minimum . map (length . shrink)
    $ zipWith (\c xs -> filter ((/=c).toLower) xs) (alphabet xs) (repeat xs)

shrink xs = go False [] xs
    where
        go again xs (x:y:ys)
            | x /= y && toLower x == toLower y = go True xs ys
            | otherwise = go again (x:xs) (y:ys)
        go False xs ys = (reverse xs)++ys
        go True xs ys  = go False [] ((reverse xs)++ys)

alphabet = S.toList . S.fromList . map toLower -- just english alphabet
