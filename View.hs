{-# LANGUAGE OverloadedStrings #-}
module View where

import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String
import Control.Monad (forM_)
import Data.List (transpose)

renderReport as t totals= renderHtml $ cmeReport as t totals

bootStrapLink = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.0/css/bootstrap.min.css"


cmeReport as t totals = docTypeHtml $ do
     H.head $ do
         H.title "CME Report"
         H.link ! rel "stylesheet" ! type_ "text/css" ! href bootStrapLink
     H.body $ do
       H.div ! class_ "row" $ do
         H.div ! class_ "col-md-3 col-md-offset-1" $ do
           H.h3 $ (toHtml $ "CME Report - generated on " ++ show t)
           H.br
           totalsTable $ map (\(h,t) -> [toHtml h, (toHtml t)]) totals 
           H.br
         H.div ! class_ "col-md-10 col-md-offset-1" $ do
           reportTable as

totalsTable ts = do
  H.table ! class_ "table table-striped" $ do
    H.h4 "Credit Hours"
    H.thead $ H.tr $ do mapM_ (\h -> H.th h) ["Type", "Hours"]
    H.tbody $ forM_ ts (\a -> reportRow' a)
 
  
reportTable as  = do
  H.table ! class_ "table table-striped" $ do
    H.h4 "Activity"
    H.thead $ H.tr $ do mapM_ (\h -> H.th h) headings
    H.tbody $ forM_ as (\a -> reportRow a)
  where
    headings = ["Title", "Description", "Resource", "Category", "Subcategory", "Modality", "Type", "Hours", "Completed"]
      
reportRow a = H.tr $ do
    forM_ a (\s -> H.td $ toHtml s)

reportRow' a = H.tr $ do
    forM_ a (\s -> H.td s)


