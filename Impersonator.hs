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

import qualified Data.Map.Strict as M
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

getGram :: Int -> [a] -> Maybe (Gram a)
getGram _ [] = Nothing
getGram 1 (h:_) = Just ([], h)
getGram n (h:t) = fmap (\(l,r) -> ((h:l), r)) $ getGram (n-1) t

ngrams :: [a] -> Int -> [Gram a]
ngrams lst n = case getGram n lst of
                Just gram -> gram : ngrams (tail lst) n
                Nothing -> []

insertGram :: (Ord a, Hashable a) => Gram a -> GramMap a -> GramMap a
insertGram (key,next) = HM.alter (Just . M.insertWith (+) next 1 . fromMaybe M.empty) key

insertGrams :: (Ord a, Hashable a) => GramMap a -> [Gram a] -> GramMap a
insertGrams = foldl' (\gm g -> insertGram g gm)

expandPredictions :: [GramPred a] -> [a]
expandPredictions [] = []
expandPredictions ((v, 0):t) = expandPredictions t
expandPredictions ((v, f):t) = v : (expandPredictions ((v, f-1):t))

longestPredictions :: (Ord a, Hashable a) => GramMap a -> [a] -> Map a Int
longestPredictions _ [] = M.empty
longestPredictions gm lst =
  case HM.lookup lst gm of
    Just fm -> fm
    Nothing -> longestPredictions gm (tail lst)

predictions :: (Ord a, Hashable a) => GramMap a -> [a] -> Map a Int
predictions _ [] = M.empty
predictions gm lst =
  let subMap = predictions gm (tail lst)
  in
    case HM.lookup lst gm of
      Just fm -> M.unionWith (\tn sn -> M.size subMap * tn + sn) fm $ predictions gm (tail lst)
      Nothing -> predictions gm (tail lst)

-- Causes an exception if there are ever no predictions
generatePopular :: (Ord a, Hashable a) => GramMap a -> [a] -> Int -> [a]
generatePopular _ lst 0 = lst
generatePopular gm lst n =
  generatePopular gm (lst ++ [fst $ head $ sortByFreq $ M.toList $ predictions gm lst]) (n-1)

frequencySelect :: [GramPred a] -> Int -> a
frequencySelect ((val, c):t) n
  | c > n = val
  | otherwise = frequencySelect t (n-c)

generateFrequency :: (Ord a, Hashable a, RandomGen g) => GramMap a -> [a] -> Int -> g -> ([a], g)
generateFrequency _ lst 0 g = (lst, g)
generateFrequency gm lst n rng =
  let preds = M.toList $ predictions gm lst
      freqSum = sum $ map snd preds
      (index, nRng) = R.randomR (0,freqSum-1) rng
      selected = frequencySelect preds index
  in
    generateFrequency gm (lst ++ [selected]) (n-1) nRng

generateRandom :: (Ord a, Hashable a, RandomGen g) => GramMap a -> [a] -> Int -> g -> ([a], g)
generateRandom _ lst 0 g = (lst, g)
generateRandom gm lst n rng =
  let preds = M.toList $ predictions gm lst
      (index, nRng) = R.randomR (0, length preds - 1) rng
      selected = fst $ preds !! index
  in
    generateRandom gm (lst ++ [selected]) (n-1) nRng

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
    putStrLn "1. Impersonate it well!"
    putStrLn "2. Impersonate it overfitted!"
    putStrLn "3. See some statistics"
    putStrLn "4. Quit"
    opt <- getLine
    let optNum = (read opt)
    case optNum of
      1 -> impersonate gramMap
      2 -> impersonateOverfit gramMap
      3 -> mostFreqNGram gramMap
      4 -> endProgram
      otherwise -> invalidOption gramMap

impersonateOverfit gramMap =
  do
    putStrLn "\nStart to impersonate (overfitted) by typing in a few words"
    inputStr <- getLine
    putStrLn "How long would you like the sentence to be?"
    predLen <- getLine
    let predicted = (generatePopular gramMap (processText inputStr) ((read predLen) - 1))
    putStrLn "\nThe sentence:"
    -- tail is to delete the unnecessary whitespace character
    putStrLn(tail (combineText predicted) ++ "\n")
    options gramMap

impersonate gramMap =
  do
    rng <- getStdGen
    putStrLn "\nStart to impersonate by typing in a few words"
    inputStr <- getLine
    putStrLn "How long would you like the sentence to be?"
    predLen <- getLine
    let processedText = processText inputStr
        predLenNum    = read predLen
        predicted     = generateFrequency gramMap processedText (predLenNum - 1) rng
    setStdGen (snd predicted)
    putStrLn "\nThe sentence:"
    -- tail is to delete the unnecessary whitespace character
    putStrLn(tail (combineText (fst predicted)) ++ "\n")
    options gramMap

mostFreqNGram gramMap =
  do
    putStrLn "\ntype in a number (N) to see the most frequent N-gram of that size! (including punctuation)"
    input <- getLine
    let gramList = HM.toList(gramMap)
        unorderedList = (temp gramList)
        orderedList = (sortByFreq unorderedList)
        num = (read input)
        mostFreq1 = (getTuple orderedList num)
        noTop1 = (delTopTuple orderedList mostFreq1)
        mostFreq2 = (getTuple noTop1 num)
        noTop2 = (delTopTuple noTop1 mostFreq2)
        mostFreq3 = (getTuple noTop2 num)
        noTop3 = (delTopTuple noTop2 mostFreq3)
        mostFreq4 = (getTuple noTop3 num)
        noTop4 = (delTopTuple noTop3 mostFreq4)
        mostFreq5 = (getTuple noTop4 num)
    putStrLn ("\n1. " ++ combineText (fst mostFreq1))
    putStrLn ("This N-gram appeared " ++ (show (snd mostFreq1)) ++ " times in the file.\n")
    putStrLn ("2. " ++ combineText (fst mostFreq2))
    putStrLn ("This N-gram appeared " ++ (show (snd mostFreq2)) ++ " times in the file.\n")
    putStrLn ("3. " ++ combineText (fst mostFreq3))
    putStrLn ("This N-gram appeared " ++ (show (snd mostFreq3)) ++ " times in the file.\n")
    putStrLn ("4. " ++ combineText (fst mostFreq4))
    putStrLn ("This N-gram appeared " ++ (show (snd mostFreq4)) ++ " times in the file.\n")
    putStrLn ("5. " ++ combineText (fst mostFreq5))
    putStrLn ("This N-gram appeared " ++ (show (snd mostFreq5)) ++ " times in the file.\n")
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

delTopTuple [] _ = []
delTopTuple (h:t) a
    | ((fst h) == (fst a) && (snd h) == (snd a)) = delTopTuple t a
    | otherwise              = h : delTopTuple t a

endProgram =
  do
    putStrLn "\nGoodbye!"
    return ()

invalidOption gramMap =
  do
    putStrLn "Invalid option! Please try again."
    options gramMap
