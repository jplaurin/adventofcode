-- file: day3.hs
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

main = interact day3

day3 = affiche.process.obtient

process xs = [a,b]
    where a             = Set.size listOfOverlap
          b             = findNoOverlap xs listOfOverlap
          listOfOverlap = (Set.unions).getListIntersect $ xs

findNoOverlap xs overlapSet = head [ noId | x<-xs, 
                                let noId = head x, 
                                let sa = getPosSet x,
                                let interSet = (Set.intersection ) sa overlapSet,
                                null interSet]

getListIntersect (x:xs) = getListLoop x xs

getListLoop _ [] = []
getListLoop x xs = nub ( [ns| sb<-ls, let ns=Set.intersection sa sb, not(null ns) ] ++
                         (getListLoop (head xs) (tail xs)) )
                where  sa = getPosSet x;
                       ls = map getPosSet xs

getPosSet :: [Int] -> Set (Int,Int)
getPosSet [_,x0,y0,w,h] = Set.fromList listOfPos
    where listOfPos = [(x,y)| x<-[x0..(x0+w-1)], 
                              y<-[y0..(y0+h-1)] ]
getPosSet _ = Set.fromList []

obtient = map (lineToFiveInt).lines

lineToFiveInt = map sToI.words.map putSpace

putSpace ',' = ' '
putSpace 'x' = ' '
putSpace ':' = ' '
putSpace '@' = ' '
putSpace '#' = ' '
putSpace c = c
  
sToI s = read s :: Int
 
affiche = unlines.map show 
