-- file: day1
import Data.Char(toUpper)
main = interact ((++"\n").show.sum.map (\s -> read s ::Integer).lines.(filter (/='+')))

