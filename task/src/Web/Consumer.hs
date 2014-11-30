module Web.Consumer (printPageSummary, nGramGenerator) where

import Control.Concurrent (modifyMVar_, withMVar, newMVar)
import Data.List (intercalate)
import Trigram.Generator
import Text.XML.HXT.Core
import Text.HandsomeSoup

import Web.Crawler (Document)

data Strict a = Strict !a

printPageSummary :: Document -> IO ()
printPageSummary doc = do
    titles <- runX $ doc >>> css "head title" /> getText
    text <- getDocumentText doc
    putStrLn $! (show . concat $ titles) ++ " " ++ (show . length $ text)
    
nGramGenerator :: IO (Document -> IO(), IO NGrams)
nGramGenerator = do
    nGrams <- newMVar $ Strict emptyNGrams
    return (addPage nGrams, retrieveNGrams nGrams)
    where addPage nGrams doc = modifyMVar_ nGrams $ \(Strict ng) -> do
              text <- getDocumentText doc
              return $ Strict $! addText text ng
          retrieveNGrams nGrams = withMVar nGrams (return . fromStrict)
          fromStrict (Strict a) = a

getDocumentText :: Document -> IO String
getDocumentText doc = do
    let textTags = ["div", "p", "span", "i", "b", "u"]
        selector = (foldr1 (<+>)) . (map css) $ textTags
    texts <- runX $ doc >>> selector /> getText
    return $ intercalate ". " texts