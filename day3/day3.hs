-- file: day3.hs
-- import Data.Set

main = interact day3

day3 = affiche.process.obtient

process = id

obtient = map (lineToFourInt).lines.filter (/=':')

lineToFourInt = map sToI.drop 2.words.map putSpace

putSpace ',' = ' '
putSpace 'x' = ' '
putSpace c = c
  
sToI s = read s :: Int
 
affiche = unlines.map show
