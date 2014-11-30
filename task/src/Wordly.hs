module Main (main) where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import Data.Binary (decodeFile)

import Trigram.Generator

data Params = Params {
    count :: Int,
    filePath :: FilePath
} deriving (Show)

usage :: String
usage = "<program> <count> <trigram file>\n\
        \program: path to the executable\n\
        \count: number of sentences to generate\n\
        \trigram file: file to retrieve the generated trigrams\n"

parseInt :: String -> Maybe Int
parseInt = checkError . reads 
    where checkError [(value, "")] = Just value
          checkError _ = Nothing

parseParams :: [String] -> Maybe Params
parseParams args = if (length args /= 2) then Nothing
                   else do
                       count <- parseInt $ args!!0
                       let filePath = args!!1
                       Just $ Params count filePath

main :: IO()
main = do
    args <- getArgs
    process $ parseParams args
    where process (Just (Params count filePath)) = do
                                nGrams <- decodeFile filePath
                                sequence_ $ take count $ map (const $ putStrLn =<< (generateSentence nGrams)) [1..]
          process Nothing = do
                                hPutStrLn stderr usage
                                exitFailure
