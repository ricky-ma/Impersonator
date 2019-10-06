import Data.Char (toLower)
import Data.Map (Map, toList, fromListWith)
import Data.List (sortBy)
import Data.Function (on)


-- splitsep (==',') "3,5,"  => ["3","5",""]
splitsep :: (a -> Bool) -> [a] -> [[a]]
splitsep sep [] = [[]]
splitsep sep (h:t)
    | sep h = []: splitsep sep t
    | otherwise = ((h:w):rest)
                where w:rest = splitsep sep t

removePunc :: String -> String
removePunc string = [ char | char <- string, not (char `elem` ",.?!-:;\"\'\n") ]

delPuncAndLower :: [String] -> [String]
delPuncAndLower strings = map (map toLower) (map removePunc strings)

sortByFreq :: Ord n => [(t, n)] -> [(t, n)]
sortByFreq tuples = sortBy (flip compare `on` (\(a,b)->b)) tuples

readtxt filename =
    do
      file <- readFile filename
      let words = concat [splitsep (==' ') line| line <- splitsep (=='\n') file]
      let cleanWords = delPuncAndLower words
      let tuples = toList (fromListWith (+) [(word, 1) | word <- cleanWords])
      return (sortByFreq tuples)
