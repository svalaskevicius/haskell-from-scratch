module Main (main) where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import Data.Binary (encodeFile)

import Web.Crawler
import Web.Consumer

data Params = Params {
    count :: Int,
    url :: String,
    filePath :: FilePath
} deriving (Show)

usage :: String
usage = "<program> <count> <url> <filePath>\n\
        \program: path to the executable\n\
        \count: number of sub-pages to follow\n\
        \url: the starting url\n\
        \filePath: path to store the trigrams to\n"

parseInt :: String -> Maybe Int
parseInt = checkError . reads 
    where checkError [(value, "")] = Just value
          checkError _ = Nothing

parseParams :: [String] -> Maybe Params
parseParams args = if (length args /= 3) then Nothing
                   else do
                       count <- parseInt $ args!!0
                       let url = args!!1
                       let filePath = args!!2
                       Just $ Params count url filePath

main :: IO()
main = do
    args <- getArgs
    process $ parseParams args
    where process (Just (Params count url filePath)) = do
                                (addNewPage, retrieveNGrams) <- nGramGenerator
                                crawl count [url] (\tags -> printPageSummary tags >> addNewPage tags)
                                encodeFile filePath =<< retrieveNGrams
          process Nothing = do
                                hPutStrLn stderr usage
                                exitFailure
