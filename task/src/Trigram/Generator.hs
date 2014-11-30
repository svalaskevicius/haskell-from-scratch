module Trigram.Generator (
    NGrams(..),
    addText,
    generateSentence,
    empty
    ) where

import Prelude hiding (lookup)
import Data.ByteString (ByteString(..))
import Data.ByteString.Char8 (pack, unpack)
import Data.Trie (Trie(..))
import Data.List (tails)
import Data.Trie (insert, singleton, lookup, empty, submap, size, keys)
import Debug.Trace (trace)
import Data.Binary (Binary(..), Get(..))
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

type NGrams = Trie NGramEndings

sentenceStart :: String
sentenceStart = "^=>"

sentenceEnd :: String
sentenceEnd = "<=^"

addText :: String -> NGrams -> NGrams
addText text ngram = foldr addSentence ngram sentences
    where sentences = breakAll (`elem` ".?!") text

-- TODO: use real probability
generateSentence :: NGrams -> IO String
generateSentence ngrams = do
    start <- getStart
    let [x1, x2] = (map pack) . words . unpack $ start
    generateNext (unpack start) x1 x2
    where getStart = do
                idx <- randomRIO (0, size startGrams - 1)
                return $ (keys startGrams)!!idx
          startGrams = submap (pack sentenceStart) ngrams
          generateNext prefix prev current = do
                let key = pack . unwords . (map unpack) $ [prev, current]
                case lookup key ngrams of
                    Just e -> do
                        let sz = size $ endings e
                        if sz == 0 then return prefix
                        else do
                            idx <- randomRIO (0, sz - 1)
                            let new = (keys $ endings e)!!idx
                            generateNext (prefix ++ " "++ (unpack new)) current new
                    Nothing -> return prefix
                





addSentence :: String -> NGrams -> NGrams
addSentence sentence ngram = foldr addTrigram ngram trigrams
    where trigrams = ngrams 3 $ sentenceStart : (words sentence) ++ [sentenceEnd]

addTrigram :: [String] -> NGrams -> NGrams
addTrigram [] ngram = ngram
addTrigram words ngram = insert key newData ngram
    where key = pack . unwords . init $ words
          lastWord = pack . last $ words
          oldData = lookup key ngram
          newData = case oldData of 
                        Just d -> d{endings = newEndings $ endings d, count = count d + 1}
                        Nothing -> NGramEndings (singleton lastWord 1) 1
          newEndings oldEndings = case lookup lastWord oldEndings of
                        Just c -> insert lastWord  (c+1) oldEndings
                        Nothing -> insert lastWord 1 oldEndings


ngrams :: Int -> [String] -> [[String]]
ngrams degree words = [take degree w | w <- tails words]


breakAll :: (a -> Bool) -> [a] -> [[a]]
breakAll _ [] = []
breakAll condition list = if null first then [nextOnes]
                          else match : breakAll condition nextOnes
    where (first, others) = break condition list
          match = first ++ takeWhile condition others
          nextOnes = dropWhile condition others
    