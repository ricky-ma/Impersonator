import Control.DeepSeq (deepseq)
import Data.Char (toLower)
import Data.Map (Map)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)
import Data.List (sortBy)
import Data.List.Split (split, whenElt)
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

-- Remove all elements that appear more than once in a row in 'seps'
removeSeqElems :: Eq a => [a] -> [a] -> [a]
removeSeqElems seps [] = []
removeSeqElems seps (h:t)
  | h `elem` seps && isNextElem t = removeSeqElems seps $ dropWhile (`elem` seps) t
  | otherwise = h : removeSeqElems seps t
      where isNextElem [] = False
            isNextElem (h:t) = h `elem` seps

-- Translate everything in 'from' to 'to' in given list
translate :: Eq a => [a] -> a -> [a] -> [a]
translate _ _ [] = []
translate from to (h:t)
  | h `elem` from = to : translate from to t
  | otherwise = h : translate from to t

separateElems :: Eq a => [a] -> [a] -> [[a]]
separateElems seps = split (whenElt (`elem` seps))

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

processText :: String -> [String]
processText =
  filter (not . null) . concat . map (separateElems ".,?!:;\"") . words . translate "*_()[]{}<>" ' ' . removeSeqElems "_-="

readtxt :: FilePath -> Int -> IO (GramMap String)
readtxt filename n =
    do
      fileText <- readFile filename
      let textWords = processText fileText
          grams = ngrams textWords n
          gramMap = insertGrams HM.empty grams

      return gramMap

main :: IO ()
main = do
  [filename, nstr] <- getArgs
  let n = read nstr :: Int
  gramMap <- readtxt filename n
  putStrLn "Parsing input file... This may take a minute."
  deepseq gramMap putStrLn "Done." -- Dark magic, ensures gramMap is resolved
  putStrLn "TODO: prompt for text and generate predictions"
