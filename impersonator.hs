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

ngrams :: [a] -> Int -> [[a]]
ngrams lst n
    | length lst >= n = take n lst : ngrams (tail lst) n
    | otherwise = []

readtxt :: (Ord n, Num n) => FilePath -> Int -> IO [[([String], n)]]
readtxt filename n =
    do
      file <- readFile filename
      let words = concat [splitsep (==' ') line| line <- splitsep (=='\n') file]
      let cleanWords = delPuncAndLower words
      let grams = map (ngrams cleanWords) [1..n]
      let tuples = map (\ xgrams -> (toList (fromListWith (+) [(gram, 1) | gram <- xgrams]))) grams
      return (map sortByFreq tuples)
