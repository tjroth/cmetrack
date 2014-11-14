module Format ( tabulate ) where

import Data.List
import Text.PrettyPrint.Boxes

------------------------------------------------
-- | Show a list of list of strings in table format
--
tabulate :: Bool       -- ^ Show Row Numbers
         -> [String]   -- ^ Column Headers
         -> [[String]] -- ^ Table Data as Strings
         -> Box
tabulate fl hdr rows = if fl then nb <+> db else db
  where
    cols = transpose ([hdr] ++ rows)
    numRows = length rows
    numStrings = map show [1 .. numRows]
    db = hcat left $ map (\c -> vcat left $ (map (\t -> text t) c)) cols 
    nb = vcat left [(text "#"), vcat left $ map (\t -> text t) numStrings] 

