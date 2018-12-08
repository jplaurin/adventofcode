-- file: day2.hs
import Data.List

main = interact day2

day2 = presente.process.obtient

process xs = (a,b)
    where a = calculeChecksum xs
          b = tryAllPair xs

calculeChecksum xs = a * b
    where a = sum.map fst $ listOccurence
          b = sum.map snd $ listOccurence
          listOccurence = map compte $ xs

compte xs= (a,b)
    where a = length.filter (==2) $ freqList
          b = length.filter (==3) $ freqList
          freqList = nub.map length.group.sort $ xs

tryAllPair xs = head [common | x<-xs,y<-xs,x<=y, let common = findCommon x y, let l = length common , l==(ml-1) ]
    where ml = length.head $ xs

findCommon xs ys = map fst.filter (\(a,b)-> a==b ).zip xs $ ys

obtient = lines

presente (a,b) = (show a) ++"\n" ++ b ++ "\n"
