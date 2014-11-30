module Web.Crawler (crawl) where

import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (catMaybes, fromJust)
import Network.URI (parseURIReference, relativeTo, parseURI, uriToString)
import Network.HTTP (simpleHTTP, getResponseBody, getResponseCode, getRequest, rspHeaders)
import Network.HTTP.Headers (lookupHeader, HeaderName(..))
import Text.HTML.TagSoup (parseTags, Tag(..), fromAttrib, isTagOpenName)
import Control.Concurrent (forkIO, modifyMVar_, threadDelay, withMVar, newMVar, MVar)
import Control.Monad ((<=<))

data ConcurentCrawlers = ConcurentCrawlers {
    crawlerCount :: Int,
    leftToCrawl :: Int,
    history :: [String],
    nextLinks :: [String]
}

data Strict a = Strict !a

crawl :: Int -> [String] -> ([Tag String] -> IO()) -> IO ()
crawl nr urls consumer = do
    crawlerInfo <- newMVar $ Strict $ ConcurentCrawlers 0 nr [] urls
    crawlConcurrently crawlerInfo consumer

crawlConcurrently :: MVar (Strict ConcurentCrawlers) -> ([Tag String] -> IO()) -> IO ()
crawlConcurrently crawlerInfo consumer = scheduler
    where scheduler = do
              (count, left, noLinks) <- withMVar crawlerInfo (\(Strict info) -> return (crawlerCount info, leftToCrawl info, null . nextLinks $ info))
              if left > 0 then do
                  if count > 20 || noLinks then threadDelay 100000
                  else modifyMVar_ crawlerInfo spawnCrawler
                  scheduler
              else if count > 0 then threadDelay 100000 >> scheduler
                   else return ()

          spawnCrawler (Strict concurentCrawlers) = do
              let url = head . nextLinks $ concurentCrawlers
              forkIO $ crawlOne url
              return $ Strict $ concurentCrawlers {
                  crawlerCount = crawlerCount concurentCrawlers + 1,
                  leftToCrawl = leftToCrawl concurentCrawlers - 1,
                  history = url : (history concurentCrawlers),
                  nextLinks = tail . nextLinks $ concurentCrawlers
              }

          crawlOne url = processResponse =<< follow url consumer

          processResponse (Just nextRawSet) = modifyMVar_ crawlerInfo (\(Strict info) -> 
                                                return $ Strict $ info{
                                                    crawlerCount = crawlerCount info - 1,
                                                    nextLinks = (prepareNextSet (history info) nextRawSet) ++ (nextLinks info)
                                                })
          processResponse Nothing = modifyMVar_ crawlerInfo (\(Strict info) -> 
                                                return $ Strict $ info{
                                                    crawlerCount = crawlerCount info - 1,
                                                    leftToCrawl = leftToCrawl info + 1
                                                })

          prepareNextSet history = (filterUrls history) . fixUrls
          

follow :: String -> ([Tag String] -> IO()) -> IO (Maybe [String])
follow url consumer = do
    resp <- simpleHTTP $ getRequest url
    code <- getResponseCode resp
    processResp code resp
    where links tags = catMaybes . map (absoluteLink . (fromAttrib "href")) $ filter isLink tags
          isLink = isTagOpenName "a"
          absoluteLink link = makeAbsoluteUrl url link

          processResp (2,_,_) resp = do 
              page <- getResponseBody resp
              let tags = parseTags page
              consumer tags
              return $ Just $ links tags
          processResp (3,_,_) resp = either (const $ return Nothing) (redirect . (absoluteLink <=< lookupHeader HdrLocation) . rspHeaders) resp
          processResp _ _ = return Nothing

          redirect (Just newUrl)
           | isHttp newUrl = follow newUrl consumer
           | otherwise = return Nothing
          redirect _ = return Nothing


makeAbsoluteUrl :: String -> String -> Maybe String
makeAbsoluteUrl base link = fmap toString linkUrl
    where baseUrl = parseURI $ base

          linkUri = parseURIReference $ link

          linkUrl = maybe (do
                            base <- baseUrl
                            link <- linkUri
                            return $ link `relativeTo` base
                        ) Just $ parseURI link

          toString url = uriToString id url ""

fixUrls :: [String] -> [String]
fixUrls =  map fixUrl
    where fixUrl = takeWhile ((/=) '#')

filterUrls :: [String] -> [String] -> [String]
filterUrls history = (filter (`notElem` history )) . (filter isHttp)

isHttp :: String -> Bool
isHttp = isPrefixOf "http://"
