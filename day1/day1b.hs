--- file: day1b.hs

main = interact ((++"\n").show.findPremierDup [0].cycle.map (\s -> read s ::Integer).lines.(filter (/='+')))

findPremierDup xs (y:ys) 
    | n `elem` xs = n
    | otherwise = findPremierDup (n:xs) ys
      where n = y + head xs
findPremierDup _ _ = 0 
