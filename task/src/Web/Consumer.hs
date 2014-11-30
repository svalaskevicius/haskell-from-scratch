module Web.Consumer (printPageSummary, nGramGenerator) where

import Text.HTML.TagSoup (Tag(..), innerText, isTagCloseName, isTagOpenName, isTagText, fromTagText, isTagPosition)
import Control.Concurrent (modifyMVar_, withMVar, newMVar, MVar(..))
import Data.List (intercalate)

import Trigram.Generator

data Strict a = Strict !a

printPageSummary :: [Tag String] -> IO()
printPageSummary tags = do
    putStrLn $ (show titleText) ++ " " ++ (show . length . innerText $ tags)
    where titleText = let head = firstRange "head" tags
                          title = firstRange "title" head
                      in innerText title
    
nGramGenerator :: IO ([Tag String] -> IO(), IO NGrams)
nGramGenerator = do
    nGrams <- newMVar $ Strict emptyNGrams
    return (addPage nGrams, retrieveNGrams nGrams)
    where addPage nGrams tags = modifyMVar_ nGrams $ \(Strict ng) -> return $ Strict $ addText (tagsToText tags) ng
          retrieveNGrams nGrams = withMVar nGrams (return . fromStrict)
          fromStrict (Strict a) = a
          
tagsToText :: [Tag String] -> String
tagsToText = (intercalate ". ") . (filter ((> 30).length)) . (map fromTagText) . (filter isTagText) . dropRanges "script" . firstRange "body"


firstRange :: String -> [Tag String] -> [Tag String]
firstRange name = takeWhile (not . (isTagCloseName name)) . dropWhile (not . (isTagOpenName name))

dropRanges :: String -> [Tag String] -> [Tag String]
dropRanges _ [] = []
dropRanges name tags = outside ++ (dropRanges name . dropWhile (isTagCloseName name) . dropWhile (not . (isTagCloseName name)) $ afterStart)
    where (outside, afterStart) = break (isTagOpenName name) tags
    