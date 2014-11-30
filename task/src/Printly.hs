module Main (main) where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import Data.Binary (decodeFile)

import Trigram.Generator

data Params = Params FilePath deriving (Show)

usage :: String
usage = "<program> <trigram file>\n\
        \program: path to the executable\n\
        \trigram file: file to retrieve the generated trigrams\n"

parseParams :: [String] -> Maybe Params
parseParams args = if (length args /= 1) then Nothing
                   else do
                       let filePath = args!!0
                       Just $ Params filePath

main :: IO()
main = do
    args <- getArgs
    process $ parseParams args
    where process (Just (Params filePath)) = do
                                nGrams <- decodeFile filePath
                                sequence_ $ map (putStrLn . show) $ nGramsInfo nGrams
          process Nothing = do
                                hPutStrLn stderr usage
                                exitFailure
