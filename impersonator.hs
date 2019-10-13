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

-- assuming N-gram data is in descending order of frequency
predict :: [([String], Integer)] -> [String] -> String
predict nData [] = "hey"
predict []     _ = "hi"
predict (h:t) input
    | (matchingGram h input) = (getPredictedString h)
    | otherwise              = (predict t input)

-- just concatenates the list of strings together
getPredictedString :: ([String], Integer) -> String
getPredictedString ([], int) = ""
getPredictedString ((h:t), int) = h ++ " " ++ getPredictedString (t, int) 

-- returns true if the gram's first n words matches with input
matchingGram :: ([String], Integer) -> [String] -> Bool
matchingGram ([], _ ) _  = True
matchingGram (_ , _ ) [] = True
matchingGram (lst, len) input = (take (length input) lst) == input

-- readtxt :: IO [[([String], Integer)]]
readtxt =
    do
      putStrLn "Please type in the file name you want to impersonate"
      fileName <- getLine
      file <- readFile fileName
      putStrLn "Please type in the number (integer) for the length you want your N-grams"
      putStrLn "The bigger the number the longer it'll take to compute (Recommended: 5 - 10)"
      nString <- getLine
      let n = (read nString)
      let words = concat [splitsep (==' ') line| line <- splitsep (=='\n') file]
      let cleanWords = delPuncAndLower words
      let grams = map (ngrams cleanWords) [1..n]
      let tuples = map (\ xgrams -> (toList (fromListWith (+) [(gram, 1) | gram <- xgrams]))) grams
      let nGramData = (map sortByFreq tuples)

      -- uncomment line below to see all data
      -- return (nGramData)
      
      putStrLn "Now, enter a word / phrase and we can impersonate the given file."
      phrase <- getLine
      let input = (splitsep (==' ') phrase)
      let cleanInput = (delPuncAndLower input)
      putStrLn "How long would you like the sentence to be?"
      putStrLn "Please input a number smaller or equals to the max N-gram length"
      nString2 <- getLine
      let len = (read nString2)
      if (len <= n) 
        then return (init (predict (nGramData !! (len - 1)) cleanInput))
        else return ("Sorry, the sentence you want made is too long.")