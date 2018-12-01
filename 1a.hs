

main = interact (show . sum . map read . map (filter (/='+')) . lines)
