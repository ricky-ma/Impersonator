import Data.Char
import Data.Map
import Data.List 
import Data.Function


-- splitsep (==',') "3,5,"  => ["3","5",""]
splitsep sep [] = [[]]
splitsep sep (h:t)
    | sep h = []: splitsep sep t
    | otherwise = ((h:w):rest)
                where w:rest = splitsep sep t

removePunc string = [ char | char <- string, not (char `elem` ",.?!-:;\"\'\n") ]

delPuncAndLower strings = Prelude.map (Prelude.map toLower) (Prelude.map removePunc strings)

sortByFreq tuples = sortBy (flip compare `on` (\(a,b)->b)) tuples

readtxt filename =
    do
      file <- readFile filename
      let words = (concat [splitsep (==' ') line| line <- splitsep (=='\n') file])
      let cleanWords = (delPuncAndLower words)
      let tuples = toList (fromListWith (+) [(word, 1) | word <- cleanWords])
      return (sortByFreq tuples)
