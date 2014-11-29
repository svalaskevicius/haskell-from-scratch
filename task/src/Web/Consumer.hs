module Web.Consumer (printPageSummary, nGramGenerator) where

import Text.HTML.TagSoup (Tag(..), innerText, isTagCloseName, isTagOpenName)
import Control.Concurrent (modifyMVar_, withMVar, newMVar)

import Trigram.Generator


printPageSummary :: [Tag String] -> IO()
printPageSummary tags = do
    putStrLn $ (show titleText) ++ " " ++ (show . length . innerText $ tags)
    where titleText = let head = firstRange "head" tags
                          title = firstRange "title" head
                      in innerText title
          firstRange name = takeWhile (not . (isTagCloseName name)) . dropWhile (not . (isTagOpenName name))
    
nGramGenerator :: IO ([Tag String] -> IO(), IO NGrams)
nGramGenerator = do
    nGrams <- newMVar empty
    return (addPage nGrams, retrieveNGrams nGrams)
    where addPage nGrams tags = modifyMVar_ nGrams $ \ng -> return $ addText (innerText tags) ng
          retrieveNGrams nGrams = withMVar nGrams return