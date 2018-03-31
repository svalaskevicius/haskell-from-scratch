import System.Environment (getArgs)
import Data.List (intercalate)
import Text.Read (readMaybe)
import Data.Maybe (catMaybes)

quicksort [] = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser = filter (< p) xs
        greater = filter (>= p) xs

extractIntegers :: [String] -> [Int]
extractIntegers = catMaybes . map readMaybe

formatString = intercalate " " . map show

sortStringsAsIntegers = formatString . quicksort . extractIntegers

main :: IO()
main = do
    args <- getArgs
    putStrLn (sortStringsAsIntegers args)
