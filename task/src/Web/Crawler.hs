module Web.Crawler (crawl, Document) where

import Data.List (isPrefixOf, nub)
import Network.URI (parseURIReference, relativeTo, parseURI, uriToString)
import Network.HTTP (simpleHTTP, getResponseBody, getResponseCode, getRequest, rspHeaders)
import Network.HTTP.Headers (lookupHeader, HeaderName(..))
import Control.Concurrent (forkIO, modifyMVar_, threadDelay, withMVar, newMVar, MVar)
import Control.Monad ((<=<))
import Text.XML.HXT.Core
import Data.Maybe (catMaybes)
import Text.HandsomeSoup
import Data.Tree.NTree.TypeDefs (NTree)

type Document = IOSLA (XIOState ()) XmlTree (NTree XNode)

data ConcurentCrawlers = ConcurentCrawlers {
    crawlerCount :: Int,
    leftToCrawl :: Int,
    history :: [String],
    nextLinks :: [String]
}

data Strict a = Strict !a

crawl :: Int -> [String] -> (Document -> IO()) -> IO ()
crawl nr urls consumer = do
    crawlerInfo <- newMVar $ Strict $ ConcurentCrawlers 0 nr [] urls
    crawlConcurrently crawlerInfo consumer

crawlConcurrently :: MVar (Strict ConcurentCrawlers) -> (Document -> IO()) -> IO ()
crawlConcurrently crawlerInfo consumer = scheduler
    where scheduler = do
              (count, leftLinks, noLinks) <- withMVar crawlerInfo (\(Strict info) -> return (crawlerCount info, leftToCrawl info, null . nextLinks $ info))
              if leftLinks > 0 then do
                  if count > 10 || noLinks then threadDelay 100000
                  else modifyMVar_ crawlerInfo spawnCrawler
                  scheduler
              else if count > 0 then threadDelay 100000 >> scheduler
                   else return ()

          spawnCrawler (Strict concurentCrawlers) = do
              let url = head . nextLinks $ concurentCrawlers
              _ <- forkIO $ crawlOne url
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
                                                    nextLinks = nub $! (prepareNextSet (history info) nextRawSet) ++ (nextLinks info)
                                                })
          processResponse Nothing = modifyMVar_ crawlerInfo (\(Strict info) -> 
                                                return $ Strict $ info{
                                                    crawlerCount = crawlerCount info - 1,
                                                    leftToCrawl = leftToCrawl info + 1
                                                })

          prepareNextSet historyUrls = (filterUrls historyUrls) . fixUrls
          

follow :: String -> (Document -> IO()) -> IO (Maybe [String])
follow url consumer = do
    resp <- simpleHTTP $ getRequest url
    code <- getResponseCode resp
    processResp code resp
    where getLinks doc = do
              links <- runX $ doc >>> css "a" >>> getAttrValue "href"
              return $ catMaybes . (map absoluteLink) $ links

          absoluteLink link = makeAbsoluteUrl url link

          processResp (2,_,_) resp = do 
              page <- getResponseBody resp
              let doc = readString [withParseHTML yes, withWarnings no] page
              consumer doc
              links <- getLinks doc
              return $ Just links
          processResp (3,_,_) resp = either (const $ return Nothing) (redirect . (absoluteLink <=< lookupHeader HdrLocation) . rspHeaders) resp
          processResp _ _ = return Nothing

          redirect (Just newUrl)
           | isHttp newUrl = follow newUrl consumer
           | otherwise = return Nothing
          redirect _ = return Nothing


makeAbsoluteUrl :: String -> String -> Maybe String
makeAbsoluteUrl base link = fmap toString linkUrl
    where getBaseUrl = parseURI $ base

          getLinkUri = parseURIReference $ link

          linkUrl = maybe (do
                            baseUrl <- getBaseUrl
                            linkUri <- getLinkUri
                            return $ linkUri `relativeTo` baseUrl
                        ) Just $ parseURI link

          toString url = uriToString id url ""

fixUrls :: [String] -> [String]
fixUrls =  map fixUrl
    where fixUrl = takeWhile ((/=) '#')

filterUrls :: [String] -> [String] -> [String]
filterUrls historyUrls = (filter (`notElem` historyUrls)) . (filter isHttp)

isHttp :: String -> Bool
isHttp = isPrefixOf "http://"
