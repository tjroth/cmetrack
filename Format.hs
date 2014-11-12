module Format where

import Data.List
import Text.PrettyPrint.Boxes
{--
mkBox rows = colBox (cols !! 0) 20
                    <+> colBox (cols !! 1) 40 --para right 40 d
                    <+> colBox (cols !! 2) 20 --para right 8 tp
                    <+> colBox (cols !! 3) 6 --para right 5 h
                    <+> colBox (cols !! 4) 30 --para right 10 tm
  where
    cols = transpose rows



mkNumberBox ns = vcat left [(text "#"), vcat left $ map (\t -> text t) ns]
--}
colBox cs w = vcat left $ map (\t -> para right w t) cs

tabulate :: Bool -> [String] -> [[String]] -> Box
tabulate fl hdr rows = if fl then
                       nb <+>  db' 
                     else
                       db'
                    
  where
    db = colBox (cols !! 0) 20
                    <+> colBox (cols !! 1) 40 --para right 40 d
                    <+> colBox (cols !! 2) 20 --para right 8 tp
                    <+> colBox (cols !! 3) 20 --para right 8 tp
                    <+> colBox (cols !! 4) 20 --para right 8 tp
                    <+> colBox (cols !! 5) 6 --para right 5 h
                    <+> colBox (cols !! 6) 30 --para right 10 tm
    cols = transpose ([hdr] ++ rows)
    numRows = length rows
    numStrings = map show [1 .. (numRows)]
    db' = hcat left $ map (\c -> vcat left $ (map (\t -> text t) c)) cols -- [(text "#"), vcat left $ map (\t -> text t) numStrings] 
    nb = vcat left [(text "#"), vcat left $ map (\t -> text t) numStrings] 
