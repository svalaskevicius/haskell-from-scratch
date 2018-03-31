import           Data.Char          (isAlphaNum)
import           Data.List          (intercalate, tails)
import           Data.Map           (Map)
import qualified Data.Map           as M (alter, empty, lookup, toList)
import           Data.Maybe         (catMaybes, fromMaybe, listToMaybe)
import           System.Environment (getArgs)
import           System.Random      (newStdGen, randoms)
import           Text.Read          (readMaybe)

import           System.IO

import           Debug.Trace        (trace)

data Tree a b = Tree !(Map a (Tree a b)) !b deriving (Show)

type NGrams = Tree String Integer

emptyNGrams = Tree M.empty 0


addNGram :: NGrams -> [String] -> NGrams
addNGram (Tree ngramsMap amount) (word:ws) =
    Tree (M.alter addToNGramsMap word ngramsMap) (amount + 1)
    where addToNGramsMap Nothing  = Just $ addNGram emptyNGrams ws
          addToNGramsMap (Just m) = Just $ addNGram m ws
addNGram (Tree ngramsMap amount) [] = Tree ngramsMap (amount + 1)

generateNextWord :: NGrams -> [String] -> Double -> Maybe String
generateNextWord (Tree _ 0) _ _ = Nothing
generateNextWord (Tree ngramsMap _) (w:ws) rand = generateNextWord (lookupWord w ngramsMap) ws rand
    where
        lookupWord w ngramsMap = fromMaybe emptyNGrams (M.lookup w ngramsMap)
generateNextWord (Tree ngramsMap amount) [] r = chooseWord (floor (r* fromIntegral amount))
    where
        chooseWord ra = fmap fst . listToMaybe . take 1 . dropWhile (\(_, a) -> a < ra) . reverse $ accumAmounts
        accumAmounts = snd $ foldl accumAmount (0, []) wordsWithAmounts
            where accumAmount (lastAmount, ret) (w, wAmount) =
                    let newAmount = wAmount + lastAmount
                    in (newAmount, (w, newAmount) : ret)
        wordsWithAmounts = map (\(w, Tree _ wAmount) -> (w, wAmount)) . M.toList $ ngramsMap

loadText :: Int -> String -> NGrams
loadText n = foldl addNGram emptyNGrams . fmap (take n) . tails . words . filtered
    where
        filtered =  nlToSpace . filter (/= '\r')
        nlToSpace []           = []
        nlToSpace ('-':'\n':s) = nlToSpace s
        nlToSpace ('\n':s)     = ' ':nlToSpace s
        nlToSpace (x:xs)       = x:nlToSpace xs

generateSample :: Int -> NGrams -> Int -> [Double] -> String
generateSample n ngrams wordLimit rands = unwords $ go wordLimit [] rands
    where
        go 0 _ _ = []
        go wLimit nLast (r:rs) = case generateNextWord ngrams (reverse nLast) r of
            Just w  -> w : go (wLimit - 1) (take (n-1) (w:nLast)) rs
            Nothing -> []

main :: IO ()
main = do
    inputHandle <- openFile "./data/combined.txt" ReadMode
    hSetEncoding inputHandle char8
    hSetEncoding stdout char8
    corp <- hGetContents inputHandle
    let triGrams = loadText 3 corp
    g <- newStdGen
    let sample = generateSample 3 triGrams 10000 (randoms g)
    putStrLn sample
