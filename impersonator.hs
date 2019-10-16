import Data.Char (toLower)
import Data.Map (Map)
-- import Data.HashMap.Lazy (HashMap)
import Data.Maybe (fromMaybe)
import Data.List (sortBy)
import Data.Function (on)

import Data.Hashable

import qualified Data.Map as M
-- import qualified Data.HashMap.Lazy as HM

type Gram a = ([a], a)
type GramPred a = (a, Int)
type GramMap a = Map [a] (Map a Int)

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

ngrams :: [a] -> Int -> [Gram a]
ngrams lst n
    | length lst >= n = (take (n-1) lst, lst !! (n-1)) : ngrams (tail lst) n
    | otherwise = []

insertGram :: (Ord a) => Gram a -> GramMap a -> GramMap a
insertGram (key,next) = M.alter (Just . M.insertWith (+) next 1 . fromMaybe M.empty) key

insertGrams :: (Ord a) => GramMap a -> [Gram a] -> GramMap a
insertGrams = foldl (\gm g -> insertGram g gm)

expandPredictions :: [GramPred a] -> [a]
expandPredictions [] = []
expandPredictions ((v, 0):t) = expandPredictions t
expandPredictions ((v, f):t) = v : (expandPredictions ((v, f-1):t))

predictNext :: Ord a => GramMap a -> [a] -> [GramPred a]
predictNext gm lst =
  case M.lookup lst gm of
    Just fm -> sortByFreq $ M.toList fm
    Nothing -> []

readtxt :: FilePath -> Int -> IO (GramMap String)
readtxt filename n =
    do
      file <- readFile filename
      let words = concat [splitsep (==' ') line| line <- splitsep (=='\n') file]
      let cleanWords = delPuncAndLower words
      let grams = ngrams cleanWords n
      let gramMap = insertGrams M.empty grams

      return gramMap

-- main :: IO ()
-- main = do
--   [filename, n] <- getArgs

