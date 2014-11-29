module Web.Crawler (crawl) where

import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (catMaybes)
import Network.HTTP (simpleHTTP, getResponseBody, getResponseCode, getRequest, rspHeaders)
import Network.HTTP.Headers (lookupHeader, HeaderName(..))
import Text.HTML.TagSoup (parseTags, Tag(..), fromAttrib, isTagOpenName)

crawl :: Int -> [String] -> ([Tag String] -> IO()) -> IO ()
crawl nr (url:urls) consumer = crawlWithHistory nr (url:urls) [] consumer


crawlWithHistory :: Int -> [String] -> [String] -> ([Tag String] -> IO()) -> IO ()
crawlWithHistory 0 _ _ _ = return ()
crawlWithHistory _ [] _ _ = return ()
crawlWithHistory nr (url:urls) history consumer = do
    putStrLn url
    processResponse =<< follow url consumer
    where processResponse (Just nextRawSet) = do
            let nextSet = (filterUrls newHistory) . fixUrls $ nextRawSet 
            crawlWithHistory (nr-1) (urls++nextSet) newHistory consumer
          processResponse Nothing = crawlWithHistory nr urls newHistory consumer
          newHistory = url:history

follow :: String -> ([Tag String] -> IO()) -> IO (Maybe [String])
follow url consumer = do
    resp <- simpleHTTP $ getRequest url
    code <- getResponseCode resp
    processResp code resp
    where links tags = map (absoluteLink . (fromAttrib "href")) $ filter isLink tags
          isLink = isTagOpenName "a"
          absoluteLink link 
           | "http://" `isPrefixOf` link = link
           | "https://" `isPrefixOf` link = link
           | otherwise = url ++ "/" ++ link
          processResp (2,_,_) resp = do 
              page <- getResponseBody resp
              let tags = parseTags page
              consumer tags
              return $ Just $ links tags
          processResp _ _ = return Nothing

fixUrls :: [String] -> [String]
fixUrls = (map fixUrl) . catMaybes . map (stripPrefix "http://")
    where fixUrl = ((++) "http://") . trimSlashes . takeWhile ((/=) '#')
          trimSlashes ('/':xs) = '/':(trimSlashes . (dropWhile ((==) '/')) $ xs)
          trimSlashes (x:xs) = x:(trimSlashes xs)
          trimSlashes [] = []

filterUrls :: [String] -> [String] -> [String]
filterUrls history = (filter (`notElem` history )) . (filter (isPrefixOf "http://"))