-- file: day2.hs
import Data.List

main = interact day2

day2 = presente.calculeChecksum.obtient

calculeChecksum xs = a * b
    where a = sum.map fst $ listOccurence
          b = sum.map snd $ listOccurence
          listOccurence = map compte $ xs

compte xs= (a,b)
    where a = length.filter (==2) $ freqList
          b = length.filter (==3) $ freqList
          freqList = nub.map length.group.sort $ xs

obtient = lines

presente = (++"\n").show
