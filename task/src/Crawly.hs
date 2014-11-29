module Main (main) where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

import Web.Crawler
import Web.Consumer

data Params = Params {
    count :: Int,
    url :: String
} deriving (Show)

usage :: String
usage = "<program> <count> <url>\n\
        \program: path to the executable\n\
        \count: number of sub-pages to follow\n\
        \url: the starting url\n"

parseInt :: String -> Maybe Int
parseInt = checkError . reads 
    where checkError [(value, "")] = Just value
          checkError _ = Nothing

parseParams :: [String] -> Maybe Params
parseParams args = if (length args /= 2) then Nothing
                   else do
                       count <- parseInt $ args!!0
                       let url = args!!1
                       Just $ Params count url

main :: IO()
main = do
    args <- getArgs
    process $ parseParams args
    where process (Just (Params count url)) = crawl count [url] printPageSummary
          process Nothing = do
                                    hPutStrLn stderr usage
                                    exitFailure
