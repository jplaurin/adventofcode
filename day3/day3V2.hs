-- file: day3.hs
import Data.List
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

main = interact day3

day3 = affiche.process.obtient

process xs = [a,b]
    where a             = (IntMap.size) listOfOverlap
          b             = findNoOverlap xs listOfOverlap
          listOfOverlap = (IntMap.filter (>1)).getAllPosMap $ xs

findNoOverlap xs overlapSet = head [ noId | x<-xs, 
                                let noId = head x, 
                                let sa = getPosMap x,
                                let interSet = (IntMap.intersection ) sa overlapSet,
                                (IntMap.null) interSet]

getAllPosMap xs = (IntMap.unionsWith (+)).map getPosMap $ xs 

getPosMap xs = (IntMap.fromAscList).map (\k -> (k,1)).getPosList $ xs                                      
getPosList :: [Int] -> [Int]
getPosList [_,x0,y0,w,h] = [pos | x<-[x0..(x0+w-1)],                 -- slow
                                 y<-[y0..(y0+h-1)],                  -- slow
                                 let pos = 1000*x + y]               -- slow
getPosSet _ = []

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
