-- file: day1
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

main = interact day1

day1 :: String -> String
day1 = presenteReponse.processFrequency.obtientChangements

processFrequency :: [Int] -> (Int,Int)
processFrequency xs = (sum xs, findPremierDup ys)
                      where ys = cycle xs

findPremierDup :: [Int] -> Int
findPremierDup xs = findLoop 0 (IntSet.fromList [0]) xs

findLoop :: Int -> IntSet -> [Int] -> Int
findLoop curFreq pastFreqSet (x:xs) 
    | IntSet.member newFreq pastFreqSet = newFreq
    | otherwise = findLoop newFreq (IntSet.insert newFreq pastFreqSet) xs
      where newFreq = x + curFreq
findLoop _ _ _ = 0 

presenteReponse :: (Int,Int) -> String
presenteReponse (a,b) = concat.map ((++"\n").show) $ [a,b]

obtientChangements :: String -> [Int]
obtientChangements = map read .lines.(filter (/='+'))

