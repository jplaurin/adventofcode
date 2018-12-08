-- file: day3.hs
-- import Data.Set

main = interact day3

day3 = affiche.process.obtient

process = id

obtient = map (lineToFourInt).lines.filter (/=':')

lineToFourInt = map sToI.words.map putSpace.unwords.drop 2.words

putSpace ',' = ' '
putSpace 'x' = ' '
putSpace c = c
  
sToI s = read s :: Int
 
affiche = unlines.map show
