module Web.Crawler (crawl) where

import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (catMaybes, fromJust)
import Network.URI (parseURIReference, relativeTo, parseURI, uriToString)
import Network.HTTP (simpleHTTP, getResponseBody, getResponseCode, getRequest, rspHeaders)
import Network.HTTP.Headers (lookupHeader, HeaderName(..))
import Text.HTML.TagSoup (parseTags, Tag(..), fromAttrib, isTagOpenName)

crawl :: Int -> [String] -> ([Tag String] -> IO()) -> IO ()
crawl nr (url:urls) consumer = crawlWithHistory nr (url:urls) [] consumer


crawlWithHistory :: Int -> [String] -> [String] -> ([Tag String] -> IO()) -> IO ()
crawlWithHistory 0 _ _ _ = return ()
crawlWithHistory _ [] _ _ = return ()
crawlWithHistory nr (url:urls) history consumer = do
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
          absoluteLink = makeAbsoluteUrl url
          processResp (2,_,_) resp = do 
              page <- getResponseBody resp
              let tags = parseTags page
              consumer tags
              return $ Just $ links tags
          processResp (3,_,_) resp = either (const $ return Nothing) (redirect . (lookupHeader HdrLocation) . rspHeaders) resp
          processResp _ _ = return Nothing
          redirect (Just newUrl) = follow (absoluteLink newUrl) consumer
          redirect _ = return Nothing

makeAbsoluteUrl :: String -> String -> String
makeAbsoluteUrl base link = uriToString id linkUrl ""
    where baseUrl = fromJust . parseURI $ base
          linkUri = fromJust . parseURIReference $ link
          linkUrl = maybe (linkUri `relativeTo` baseUrl) id $ parseURI link

fixUrls :: [String] -> [String]
fixUrls =  map fixUrl
    where fixUrl = takeWhile ((/=) '#')

filterUrls :: [String] -> [String] -> [String]
filterUrls history = (filter (`notElem` history )) . (filter (isPrefixOf "http://"))