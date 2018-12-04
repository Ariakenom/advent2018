import qualified Data.Map as M
import Data.List (foldl', sort)

-- wrong 61504
-- copy paste error ==3 was ==2

main = interact (
    show
    . solve
    . lines
    )

solve = uncurry (*) . foldl' (\(a0,b0) line -> let (a1,b1) = solveLine line in (a0+a1,b0+b1)) (0,0)

solveLine line =
    let
        m = foldl' (\m c -> M.insertWith (+) c 1 m) M.empty line
        two = any (==2) (M.elems m)
        three = any (==3) (M.elems m)
    in (fromEnum two, fromEnum three)
