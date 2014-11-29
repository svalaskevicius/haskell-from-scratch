module Web.Consumer (printPageSummary) where

import Text.HTML.TagSoup (Tag(..), innerText, isTagCloseName, isTagOpenName)

printPageSummary :: [Tag String] -> IO()
printPageSummary tags = do
    putStrLn $ (show titleText) ++ " " ++ (show . length . innerText $ tags)
    where titleText = let head = firstRange "head" tags
                          title = firstRange "title" head
                      in innerText title
          firstRange name = takeWhile (not . (isTagCloseName name)) . dropWhile (not . (isTagOpenName name))
    
