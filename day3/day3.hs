-- file: day3.hs
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

main = interact day3

day3 = affiche.process.obtient

process xs = (Set.size).(Set.unions).getListIntersect $ xs

getListIntersect (x:xs) = getListLoop x xs

getListLoop _ [] = []
getListLoop x xs = nub ( [ns| sb<-ls, let ns=Set.intersection sa sb, not(null ns) ] ++
                         (getListLoop (head xs) (tail xs)) )
                where  sa = getPosSet x;
                       ls = map getPosSet xs

getPosSet :: [Int] -> Set (Int,Int)
getPosSet [x0,y0,w,h] = Set.fromList listOfPos
    where listOfPos = [(x,y)| x<-[x0..(x0+w-1)], 
                              y<-[y0..(y0+h-1)] ]
getPosSet _ = Set.fromList []

obtient = map (lineToFourInt).lines.filter (/=':')

lineToFourInt = map sToI.drop 2.words.map putSpace

putSpace ',' = ' '
putSpace 'x' = ' '
putSpace c = c
  
sToI s = read s :: Int
 
affiche x = unlines [show x]
