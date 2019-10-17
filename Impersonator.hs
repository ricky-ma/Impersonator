import Data.Char (toLower)
import Data.Map (Map)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)
import Data.List (sortBy)
import Data.Function (on)
import System.Environment (getArgs)

import Data.Hashable

import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM

type Gram a = ([a], a)
type GramPred a = (a, Int)
type GramMap a = HashMap [a] (Map a Int)

-- splitsep (==',') "3,5,"  => ["3","5",""]
splitsep :: (a -> Bool) -> [a] -> [[a]]
splitsep sep [] = [[]]
splitsep sep (h:t)
    | sep h = []: splitsep sep t
    | otherwise = ((h:w):rest)
                where w:rest = splitsep sep t

removePunc :: String -> String
removePunc string = [ char | char <- string, not (char `elem` ",.?!-_:;\"\'\n") ]

delPuncAndLower :: [String] -> [String]
delPuncAndLower strings = map (map toLower) (map removePunc strings)

sortByFreq :: Ord n => [(t, n)] -> [(t, n)]
sortByFreq tuples = sortBy (flip compare `on` (\(a,b)->b)) tuples

listHasN :: Int -> [a] -> Bool
listHasN 0 _ = True
listHasN n [] = False
listHasN n (h:t) = listHasN (n-1) t

ngrams :: [a] -> Int -> [Gram a]
ngrams lst n
    | listHasN n lst = (take (n-1) lst, lst !! (n-1)) : ngrams (tail lst) n
    | otherwise = []

insertGram :: (Ord a, Hashable a) => Gram a -> GramMap a -> GramMap a
insertGram (key,next) = HM.alter (Just . M.insertWith (+) next 1 . fromMaybe M.empty) key

insertGrams :: (Ord a, Hashable a) => GramMap a -> [Gram a] -> GramMap a
insertGrams = foldl (\gm g -> insertGram g gm)

expandPredictions :: [GramPred a] -> [a]
expandPredictions [] = []
expandPredictions ((v, 0):t) = expandPredictions t
expandPredictions ((v, f):t) = v : (expandPredictions ((v, f-1):t))

predictNext :: (Ord a, Hashable a) => GramMap a -> [a] -> [GramPred a]
predictNext gm lst =
  case HM.lookup lst gm of
    Just fm -> sortByFreq $ M.toList fm
    Nothing -> []

readtxt :: FilePath -> Int -> IO (GramMap String)
readtxt filename n =
    do
      file <- readFile filename
      let words = concat [splitsep (==' ') line| line <- splitsep (=='\n') file]
      let cleanWords = delPuncAndLower words
      let grams = ngrams cleanWords n
      let gramMap = insertGrams HM.empty grams

      return gramMap

main :: IO ()
main = do
  [filename] <- getArgs
  gramMap <- readtxt filename 3
  print $ predictNext gramMap ["whan", "that"]

