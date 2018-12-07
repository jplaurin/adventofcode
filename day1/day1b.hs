--- file: day1b.hs
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

main = interact ((++"\n").show.findPremierDup.cycle.map (\s -> read s ::Int).lines.(filter (/='+')))

findPremierDup xs = findLoop 0 (IntSet.fromList [0]) xs

findLoop acc s (y:ys) 
    | IntSet.member n s = n
    | otherwise = {-# SCC "chercheDup" #-} findLoop n (IntSet.insert n s) ys
      where n = y + acc
findLoop _ _ _ = 0 
