-- file: day3.hs
import Data.List
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

main = interact day3

day3 = affiche.process.obtient

process xs = [a,b]
    where a             = IntSet.size listOfOverlap
          b             = findNoOverlap xs listOfOverlap
          listOfOverlap = (IntSet.unions).getListIntersect $ xs

findNoOverlap xs overlapSet = head [ noId | x<-xs, 
                                let noId = head x, 
                                let sa = getPosSet x,
                                let interSet = (IntSet.intersection ) sa overlapSet,
                                (IntSet.null) interSet]

getListIntersect (x:xs) = getListLoop x xs []

getListLoop _ [] res = res
getListLoop x xs res = getListLoop (head xs) (tail xs) newRes    -- slow
                where  sa = getPosSet x;
                       ls = map getPosSet xs                      -- slow
                       newRes = (res ++ curRes)
                       curRes = [ns | sb<-ls, 
                                      let ns=IntSet.intersection sa sb, 
                                      not((IntSet.null) ns) ]

getPosSet :: [Int] -> IntSet
getPosSet [_,x0,y0,w,h] = posSet
    where posSet    = IntSet.fromAscList listOfPos
          listOfPos = [pos | x<-[x0..(x0+w-1)],                   -- slow
                              y<-[y0..(y0+h-1)],
                              let pos = 1024*x + y]
getPosSet _ = IntSet.fromList []

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
