import Control.DeepSeq (deepseq)
import Data.Char (toLower)
import Data.Map (Map)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)
import Data.List (sortBy, foldl')
import Data.List.Split (split, whenElt)
import Data.Function (on)
import System.Environment (getArgs)
import System.Random (Random, RandomGen, getStdGen, setStdGen)

import Data.Hashable

import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import qualified System.Random as R

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
insertGrams = foldl' (\gm g -> insertGram g gm)

expandPredictions :: [GramPred a] -> [a]
expandPredictions [] = []
expandPredictions ((v, 0):t) = expandPredictions t
expandPredictions ((v, f):t) = v : (expandPredictions ((v, f-1):t))

predictions :: (Ord a, Hashable a) => GramMap a -> [a] -> [GramPred a]
predictions _ [] = []
predictions gm lst =
  case HM.lookup lst gm of
    Just fm -> M.toList fm
    Nothing -> predictions gm (tail lst)

-- This causes an exception if there are no predictions, maybe use Maybe?
-- predictNextBest :: (Ord a, Hashable a) => GramMap a -> [a] -> a
-- predictNextBest gm =
--   fst . head . sortByFreq . predictions gm

-- Also causes an exception if there are no predictions
-- predictNextRand :: (Ord a, Hashable a, RandomGen g) => GramMap a -> [a] -> g -> (a, g)
-- predictNextRand gm lst rng =
--   let preds = predictions gm lst
--       freqSum = sum $ map snd preds
--       (index, nRng) = R.randomR (0,freqSum-1) rng
--   in
--     (expandPredictions preds !! index, nRng)

-- Causes an exception if there are ever no predictions
predictBest :: (Ord a, Hashable a) => GramMap a -> [a] -> Int -> [a]
predictBest _ lst 0 = lst
predictBest gm lst n =
  predictBest gm (lst ++ [fst $ head $ sortByFreq $ predictions gm lst]) (n-1)

predictRand :: (Ord a, Hashable a, RandomGen g) => GramMap a -> [a] -> Int -> g -> ([a], g)
predictRand _ lst 0 g = (lst,g)
predictRand gm lst n rng =
  let preds = predictions gm lst
      freqSum = sum $ map snd preds
      (index, nRng) = R.randomR (0,freqSum-1) rng
  in
    predictRand gm (lst ++ [expandPredictions preds !! index]) (n-1) nRng

getFirst :: (x,y) -> x
getFirst (x,y) = x

getSecond :: (x,y) -> y
getSecond (x,y) = y

combineText :: [String] -> String
combineText [] = ""
combineText ([e]:t)
  | e `elem` ",.?!:;\"" = [e] ++ combineText t
  | otherwise = [' ', e] ++ combineText t
combineText (h:t) =
  ' ' : h ++ combineText t

processText :: String -> [String]
processText =
  filter (not . null) . concat . map (separateElems ".,?!:;\"") . words . translate "*_()[]{}<>" ' ' . removeSeqElems "_-="

readtxt :: FilePath -> Int -> IO (GramMap String)
readtxt filename n =
    do
      fileText <- readFile filename
      let textWords = processText fileText
          grams = map (ngrams textWords) [2..n]
          gramMap = foldl' insertGrams HM.empty grams

      return gramMap

main =
  do
    putStrLn "Welcome to Impersonator, please type in the file name (XYZ.txt) that you want to impersonate."
    filename <- getLine
    putStrLn "Now, type in an integer of how long you want the N-gram to be.\nThe bigger the number or the file, the longer it will take to process."
    ngramLen <- getLine
    let n = read ngramLen :: Int
    gramMap <- readtxt filename n
    putStrLn "Parsing input file... This may take a minute."
    deepseq gramMap putStrLn "Done." -- Dark magic, ensures gramMap is resolved
    options gramMap

options gramMap =
  do
    putStrLn "\nType in the option (number) to proceed"
    putStrLn "1. Impersonate it!"
    putStrLn "2. Impersonate at random! (improved)"
    putStrLn "3. See some statistics"
    putStrLn "4. Quit"
    opt <- getLine
    let optNum = (read opt)
    case optNum of
      1 -> impersonateB gramMap
      2 -> impersonateR gramMap
      3 -> mostFreqNGram gramMap
      4 -> endProgram
      otherwise -> invalidOption gramMap

impersonateB gramMap = 
  do
    putStrLn "\nStart to impersonate by typing in a few words"
    inputStr <- getLine
    putStrLn "How long would you like the sentence to be?"
    predLen <- getLine
    let predicted = (predictBest gramMap (processText inputStr) ((read predLen) - 1))
    putStrLn "\nThe sentence:"
    -- tail is to delete the unnecessary whitespace character
    putStrLn(tail (combineText predicted) ++ "\n")
    options gramMap

impersonateR gramMap = 
  do
    rng <- getStdGen
    putStrLn "\nStart to impersonate (randomly) by typing in a few words"
    inputStr <- getLine
    putStrLn "How long would you like the sentence to be?"
    predLen <- getLine
    let processedText = processText inputStr
        predLenNum    = (read predLen)
        predicted     = (predictRand gramMap processedText (predLenNum - 1) rng)
    setStdGen (getSecond predicted)
    putStrLn (combineText (getFirst predicted))
    options gramMap

mostFreqNGram gramMap =
  do
    putStrLn "\ntype in a number (N) to see the most frequent N-gram of that size! (including punctuation)"
    input <- getLine
    let gramList = HM.toList(gramMap)
        unorderedList = (temp gramList)
        orderedList = (sortByFreq unorderedList)
        mostFreqTuple = getTuple orderedList (read input)
    putStrLn (combineText (getFirst mostFreqTuple))
    putStrLn ("This N-gram appeared " ++ (show (getSecond mostFreqTuple)) ++ " times in the file.")
    options gramMap

-- getting an unodered gram List
temp [] = []
temp ((x,y):t)
  | (length (M.toList (y)) == 0)  = temp t
  | otherwise = temp2 (x, (M.toList y)) ++ temp t

temp2 (x, []) = []
temp2 (x,((str,freq):t)) = (x ++ [str], freq) : temp2 (x,t)

getTuple ((a,b):y) n
    | (length a) == n = (a,b)
    | otherwise = getTuple y n

endProgram =
  do 
    putStrLn "\nGoodbye!"
    return ()
    
invalidOption gramMap =
  do
    putStrLn "Invalid option! Please try again."
    options gramMap
