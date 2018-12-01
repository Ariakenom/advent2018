import qualified Data.Set as S

main = interact (
    show
    . firstRepeat S.empty . scanl (+) 0 . cycle
    . map read . map (filter (/='+')) . lines
    )

firstRepeat set (x:xs)
    | S.member x set = x
    | otherwise      = firstRepeat (S.insert x set) xs
