-- file: day4.hs
import Data.Char
import Data.List

main = interact day4

day4 = affiche.process.obtient

process = id

affiche = unlines.map show
obtient = map lineTo5Value .sort.lines
lineTo5Value = wordsToValues.words.map punctuationToSpace

punctuationToSpace x 
    | not.isAlphaNum $ x = ' '
    | otherwise = x
    
wordsToValues (y:m:d:h:mi:t:g:_)
    | t == "Guard" = (y,m,d,h,mi,read g,0)
    | t == "falls" = (y,m,d,h,mi,0,1)
    | t == "wakes" = (y,m,d,h,mi,0,0)

