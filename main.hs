import Options.Applicative
import System.IO
import System.Directory
import Data.Time
import qualified Text.PrettyPrint.Boxes as BX
import Format
import Data.Char
import Text.Read
import Prompt
import Control.Applicative ((<$>), (<*>))
import View

data Options = Options { file :: FilePath
                       , optCommand :: Command } deriving (Show, Ord, Eq)

data Command = List
             | Add
             | Edit
             | Delete
             | Report
             deriving (Show, Ord, Eq)

data ActivityFile = ActivityFile { activities :: [Activity] } deriving (Show, Read, Eq)

data Activity = Activity { title :: String
                         , description :: String
                         , resource :: String
                         , category :: String
                         , subcategory :: String
                         , modality :: String
                         , cmeType :: CMEType
                         , hours :: Double
                         , date :: Day } deriving (Show, Read, Ord, Eq)

data CMEType = Cat1 | SACME | SAM deriving (Read, Show, Ord, Eq, Bounded, Enum)

prettyActivity :: Activity -> String
prettyActivity (Activity t d r c sc m tp h ct) = concat [ "Title: " ++ t ++ "\n"
                                                        , "Description: " ++ d ++ "\n"
                                                        , "Resource: " ++ r ++ "\n"
                                                        , "Category: " ++ c ++ "\n"
                                                        , "SubCategory: " ++ sc ++ "\n"
                                                        , "Modality: " ++ m ++ "\n"
                                                        , "Type: " ++ (show tp) ++ "\n"
                                                        , "Hours: " ++ (show h) ++ "\n"
                                                        , "Date: " ++ (show ct)]
initActivity :: Day -> Activity
initActivity d = Activity "" "" "" "" "" "" Cat1 2.0 d

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  o <- execParser optParser'
  let f = file o
  case optCommand o of
    List   -> listActivity f 
    Add    -> addActivity f
    Edit   -> editActivity f
    Delete -> deleteActivity f
    Report -> reportActivity f


optParser' :: ParserInfo Options
optParser' = info (helper <*> optParser) ( fullDesc <> header "CME tracking the Haskell way!")


optParser :: Parser Options
optParser = Options
            <$> strOption ( short 'f' <> long "file" <> metavar "FILENAME"
                            <> help "File containing the cme activities." )
            <*> subparser (command "list" (info (pure List)
                 (progDesc "List all CME activity"))
            <> command "add" (info (pure Add)
                 (progDesc "Add an activity"))
            <> command "edit" (info (pure Edit)
                 (progDesc "Edit an activity"))
            <> command "delete" (info (pure Delete)
                 (progDesc "Delete an activity"))
            <> command "report" (info (pure Report)
                 (progDesc "Generate HTML report")))


printPrettyTable :: Bool -> [Activity] -> IO ()
printPrettyTable f as = BX.printBox $ tabulate f tHeader (map toList as) 
  where
    tHeader = ["Title", "Description", "Resource", "Category", "Sub-Category", "Modality", "Type", "Hours", "Date"]  


listActivity :: FilePath -> IO ()
listActivity fp = do
  ls <- fmap lines $ readFile fp --"cme.ht"
  let as = map (\a -> read a :: Activity) ls
  printPrettyTable False as
  putStrLn "\nHours:"
  mapM_ (\ht -> putStrLn $ (show ht ++ ": " ++ (show $ totalHours ht as))) [Cat1 ..]
  putStrLn $ "Total: " ++ (show $ sum $ map (\ht -> totalHours ht as) [Cat1 ..])                           
  
addActivity :: FilePath -> IO ()
addActivity fp = do
  fe <- fileExists fp
  if (not fe) then do
    ans <- promptYesNo "File does not exist. Shall I create it?" (Just True)
    case ans of
        False -> return ()
        True -> procede fp
    else
     procede fp
  where
    procede fp = do 
     ct <- fmap localDay getLocalTime
     act <- getActivity (initActivity ct) 
     putStrLn "\nCreate Activity:"
     putStrLn $ prettyActivity act
     answer <- promptYesNo "Save Activity (y/n)" (Just True)
     case answer of
        True -> do
           putStrLn $ "Saving to \"" ++ fp ++ "\"..."
           appendFile fp ((show act) ++ "\n")
           listActivity fp
        False -> putStrLn "Goodbye"


editActivity :: FilePath -> IO ()
editActivity fp = do
  fe <- fileExists fp
  if (not fe) then do
    putStr $ "No such file \"" ++ fp ++ "\".\n" 
    else
     procede fp
  where
    procede fp = do
      handle <- openFile fp ReadMode
      contents <-fmap lines $ hGetContents handle
      let acts = map (\a -> read a :: Activity) contents
      printPrettyTable True acts
      putStr "Choose an activity to delete: "
      l <- promptInt "Choose an activity to edit" Nothing
      let act =  acts !! (l-1)
      cAct <- getActivity act
      putStrLn "\nEdited Activty:"
      putStrLn $ prettyActivity cAct
      ans <- promptYesNo "Save changes to activity" (Just True)
      case ans of
        True -> do
          let newActs = (removeActivity act acts) ++ [cAct]
          saveToFile newActs (handle, fp)
          listActivity fp
        False -> return ()


deleteActivity :: FilePath -> IO ()
deleteActivity fp = do
  handle <- openFile fp ReadMode
  contents <-fmap lines $ hGetContents handle
  let acts = map (\a -> read a :: Activity) contents
  printPrettyTable True acts
  putStr "Choose an activity to delete: "
  l <- getLine
  case (readMaybe l :: Maybe Int) of
    Just x -> do
       if x < (length acts + 1) then do
         ans <- promptYesNo "Are you sure (y/n)" (Just False)
         case ans of
           True -> do
             let newActs = (removeActivity (acts !! (x-1)) acts)
             saveToFile newActs (handle, fp)
             listActivity fp
           False -> return ()
        else
        invalidEntry >> deleteActivity fp
    Nothing -> invalidEntry >> deleteActivity fp


reportActivity :: FilePath -> IO ()
reportActivity fp = do
  cd <- fmap localDay getLocalTime
  acts <- loadActivities fp
  let sActs = map toList acts
  let totals = map (\tp -> (show tp, totalHours tp acts)) [Cat1 ..]
  writeFile ("cmereport" ) (renderReport sActs cd totals) -- ++ (show ct)) renderReport
    
loadActivities fp = do
  ls <- fmap lines $ readFile fp --"cme.ht"
  let as = map (\a -> read a :: Activity) ls
  return as

removeActivity :: Activity -> [Activity] -> [Activity]
removeActivity a as = filter (\ac -> a /= ac) as


toList :: Activity -> [String]                    
toList (Activity t d r c sc m ct h tm) = [t, d, r, c, sc, m, (show ct), (show h), (show tm)]

getActivity ::  Activity -> IO Activity
getActivity (Activity t d r c sc m tp h ct) = Activity <$> promptStr "Title" (Just t)
                                         <*> promptStr "Description" (Just d)
                                         <*> promptStr "Resource" (Just r)
                                         <*> promptStr "Category" (Just c)
                                         <*> promptStr "Sub-Category" (Just sc)
                                         <*> promptStr "Modality" (Just m)
                                         <*> getType (Just tp)
                                         <*> promptNum "Hours" (Just h)
                                         <*> return ct


getType :: Maybe CMEType -> IO CMEType
getType tp = do
  ans <- promptList "CME Type" [Cat1 ..] tp show False
  case ans of
    Left e -> return Cat1 -- shouldn't get called could change to Maybe
    Right a -> return a


invalidEntry :: IO ()
invalidEntry = do
  putStrLn "\nInvalid entry, try again."


getLocalTime :: IO LocalTime
getLocalTime = do
  u <- getCurrentTime
  z <- getCurrentTimeZone
  return $ utcToLocalTime z u
  
  
fileExists :: FilePath -> IO  Bool
fileExists fp = do
  query <- (findFile ["."] fp)
  case query of
    Just f -> return True
    Nothing -> return False


saveToFile :: [Activity] -> (Handle, FilePath) ->  IO ()
saveToFile acts (handle, fp) = do
  (tempName, tempHandle) <- openTempFile "." "temp"
  mapM_ (hPutStrLn tempHandle) $ map show acts
  hClose handle
  hClose tempHandle
  removeFile fp
  renameFile tempName fp

totalHours tp as = sum hrsForType
  where
    hrsForType = map hours $ filter (\a -> tp == (cmeType a)) as
