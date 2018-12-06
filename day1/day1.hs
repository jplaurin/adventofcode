-- file: day1

main = interact ((++"\n").show.sum.map (\s -> read s ::Integer).lines.(filter (/='+')))

