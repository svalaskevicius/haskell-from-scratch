module Trigram.Generator (
    NGrams(..),
    addText,
    generateSentence,
    emptyNGrams
    ) where

import Prelude hiding (lookup)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Data.Trie (Trie)
import Data.List (tails, intercalate)
import Data.Trie (insert, singleton, lookup, empty, size, keys, toList)
import Data.Binary (Binary(..), Get)
import System.Random (randomRIO)

data NGramEndings = NGramEndings {
    endings :: Trie Int,
    count :: Int
} deriving (Show)

instance Binary NGramEndings where
    get = do
        c <- get :: Get Int
        e <- get :: Get (Trie Int)
        return $ NGramEndings e c
    put (NGramEndings e c) = put c >> put e

data NGrams = Leaf NGramEndings | Branch (Trie NGrams) deriving (Show)

instance Binary NGrams where
    get = do
        c <- get :: Get Int
        case c of
            1 -> return . Leaf =<< (get :: Get (NGramEndings))
            2 -> return . Branch =<< (get :: Get (Trie NGrams))
            _ -> fail "unexpected symbol while decoding NGrams!"
    put (Leaf t) = put (1::Int) >> put t
    put (Branch t) = put (2::Int) >> put t


emptyNGrams :: NGrams
emptyNGrams = Branch empty

sentenceStart :: String
sentenceStart = "^=>"

sentenceEnd :: String
sentenceEnd = "<=^"

addText :: String -> NGrams -> NGrams
addText text ngram = foldr addSentence ngram sentences
    where sentences = breakAll (`elem` ".?!") text

generateSentence :: NGrams -> IO String
generateSentence ng = do
    start <- getStart ng
    let startingWords = (intercalate " ") . (map unpack) . tail $ start
    generateNext startingWords start
    where generateNext prefix key@[_, current] = case lookupFromNgrams key ng of
              Just e -> do
                  new <- selectSuffix e
                  let newString = unpack new
                  if newString == sentenceEnd then return prefix
                  else generateNext (prefix ++ " "++ newString) [current, new]
              Nothing -> return prefix
          generateNext _ _ = return ""

selectSuffix :: NGramEndings -> IO ByteString
selectSuffix (NGramEndings{count = 0}) = return $ pack sentenceEnd
selectSuffix (NGramEndings{endings = ends, count = c}) = do
    needle <- randomRIO (0, c-1)
    return $ findEnding needle $ toList ends
    where findEnding needle ((bs, reps):others)
           | needle < reps = bs
           | otherwise = findEnding (needle - reps) others
          findEnding _ _ = pack sentenceEnd

getStart :: NGrams -> IO [ByteString]
getStart (Branch b) = do
        subkey <- randomSubKey $ lookup startingKey b
        return $ startingKey : subkey
    where randomSubKey (Just (Branch ng)) = do
              let sz = size ng
              idx <- randomRIO (0, sz - 1)
              return [(keys ng)!!idx]
          randomSubKey _ = return []
          startingKey = pack sentenceStart
getStart _ = return []





addSentence :: String -> NGrams -> NGrams
addSentence sentence ngram = foldr addTrigram ngram trigrams
    where trigrams = ngrams 3 $ sentenceStart : (words sentence) ++ [sentenceEnd]

addTrigram :: [String] -> NGrams -> NGrams
addTrigram [] ngram = ngram
addTrigram wordList ngram = insertToNgrams key newData ngram
    where key = (map pack) . init $ wordList

          lastWord = pack . last $ wordList

          oldData = lookupFromNgrams key ngram

          newData = case oldData of 
                        Just d -> d{endings = newEndings $ endings d, count = count d + 1}
                        Nothing -> NGramEndings (singleton lastWord 1) 1

          newEndings oldEndings = case lookup lastWord oldEndings of
                        Just c -> insert lastWord  (c+1) oldEndings
                        Nothing -> insert lastWord 1 oldEndings


insertToNgrams :: [ByteString] -> NGramEndings -> NGrams -> NGrams
insertToNgrams [k] newData (Branch b) = Branch $ insert k (Leaf newData) b
insertToNgrams (k:key) newData (Branch b) = Branch $ maybe 
                                                (insert k (insertToNgrams key newData emptyNGrams) b) 
                                                (\ng -> (insert k (insertToNgrams key newData ng) b))
                                                $ lookup k b
insertToNgrams _ _ ng = ng

lookupFromNgrams :: [ByteString] -> NGrams -> Maybe NGramEndings
lookupFromNgrams [] (Leaf l) = Just l
lookupFromNgrams (k:key) (Branch b) = lookup k b >>= lookupFromNgrams key
lookupFromNgrams _ _ = Nothing

ngrams :: Int -> [String] -> [[String]]
ngrams degree wordList = filter ((== degree) . length) $ [take degree w | w <- tails wordList]


breakAll :: (a -> Bool) -> [a] -> [[a]]
breakAll _ [] = []
breakAll condition list = if null first then [nextOnes]
                          else match : breakAll condition nextOnes
    where (first, others) = break condition list

          match = first ++ takeWhile condition others

          nextOnes = dropWhile condition others
    