import System.Environment
import System.Exit
import System.Console.GetOpt
import qualified Data.Map as M
import Data.Char (isSpace)
import Text.Regex.Posix

type Year = Int
type Month = Int
type Day = Int

type Pattern = String

type ParseErrorMsg = String

type FileContent = String

data WorkoutLine = Date SimpleDate | Amounts [Int] | Comment String

data SimpleDate = SimpleDate Year Month Day deriving (Eq, Ord)

data Options = Options
    { optShowVersion :: Bool
    , optShowHelp :: Bool
    , optStartDate :: Maybe (Either ParseErrorMsg SimpleDate)
    , optEndDate :: Maybe (Either ParseErrorMsg SimpleDate) }

usage = "Usage: wparser [-Vhse] [file ...]"
version = "Haskell wparser 0.1"

lineDatePattern = "^#\\s*([0-9]{2}).([0-9]{2}).([0-9]{4})\\s*$"
lineAmountPattern = "^\\*\\s*([0-9]+).*$"
inputDatePattern = "^([0-9]{2}).([0-9]{2}).([0-9]{4})$"

defaultOptions = Options
    { optShowVersion = False
    , optShowHelp = False
    , optStartDate = Nothing
    , optEndDate = Nothing }

exit :: IO a
exit = exitWith ExitSuccess

die :: IO a
die = exitWith (ExitFailure 1)

trim :: String -> String
trim = let f = reverse . dropWhile isSpace in f . f

parseDate :: Pattern -> String -> Either ParseErrorMsg SimpleDate
parseDate pattern dateStr = case dateStr =~ pattern :: (String,String,String,[String]) of
                              ("",_,_,day:month:year:[]) -> Right (SimpleDate (read year) (read month) (read day))
                              (str,_,_,_) -> Left ("String '" ++ str ++ "' could not be matched for date")

options :: [OptDescr (Options -> Options)]
options = [ Option ['V'] ["version"] (NoArg (\opts -> opts { optShowVersion = True })) "show version number"
          , Option ['h'] ["help"] (NoArg (\opts -> opts { optShowHelp = True })) "show help"
          , Option ['s'] ["start"] (ReqArg (\str opts -> opts { optStartDate = Just (parseDate inputDatePattern (trim str)) }) "dd.MM.yyyy") "start date"
          , Option ['e'] ["end"] (ReqArg (\str opts -> opts { optEndDate = Just (parseDate inputDatePattern (trim str)) }) "dd.MM.yyyy") "end date" ]

parseArgs :: [String] -> IO ([FilePath], [(Options -> Options)])
parseArgs args = case getOpt Permute options args of
                   (optionFns, filePaths, []) -> return (filePaths, optionFns)
                   (_, _, errs) -> mapM putStrLn errs >> die

processOptions :: [(Options -> Options)] -> IO (SimpleDate -> Bool)
processOptions optFns = case foldl (\acc optFn -> optFn acc) defaultOptions optFns of
                          Options { optShowHelp = True } -> putStrLn usage >> exit
                          Options { optShowVersion = True } -> putStrLn version >> exit
                          Options { optStartDate = Just (Left err) } -> putStrLn err >> die
                          Options { optEndDate = Just (Left err) } -> putStrLn err >> die
                          Options { optStartDate = Just (Right startDate)
                                  , optEndDate = Just (Right endDate) } -> return (\date -> and [ or . (\ord -> (ord ==) `map` [EQ,LT]) $ startDate `compare` date
                                                                                                , or . (\ord -> (ord ==) `map` [EQ,GT]) $ endDate `compare` date ])
                          Options { optStartDate = Just (Right startDate) } -> return ((\ord -> or $ (ord ==) `map` [EQ,LT]) . compare startDate)
                          Options { optEndDate = Just (Right endDate) } -> return ((\ord -> or $ (ord ==) `map` [EQ,GT]) . compare endDate)
                          Options { optStartDate = Nothing
                                  , optEndDate = Nothing } -> return (const True)

readInputsStream :: [FilePath] -> IO FileContent
readInputsStream [] = getContents
readInputsStream filePaths = readFile `mapM` filePaths >>= (\s -> return (concat s))

convertInputsStream :: FileContent -> [WorkoutLine]
convertInputsStream = let convertLine line = case line =~ lineDatePattern :: (String,String,String,[String]) of
                                               (_,_,_,day:month:year:[]) -> Date (SimpleDate (read year) (read month) (read day))
                                               _ -> case line =~ lineAmountPattern :: (String,String,String,[String]) of
                                                      (_,_,_,amount:[]) -> Amounts [read amount]
                                                      _ -> Comment line
                      in map convertLine . lines

parseInputs :: [WorkoutLine] -> M.Map SimpleDate [Int]
parseInputs = let process (map, (Date date)) (Amounts amounts) = ((M.insertWith (++) date amounts map), (Date date))
                  process (map, _) (Date date) = (map, (Date date))
                  process (map, line) _ = (map, line)
              in fst . foldl process (M.empty, (Comment ""))

filterWorkoutMap :: (SimpleDate -> Bool) -> M.Map SimpleDate [Int] -> M.Map SimpleDate [Int]
filterWorkoutMap filterFn = M.filterWithKey (\k v -> filterFn k)

sumWorkoutMap :: M.Map SimpleDate [Int] -> Int
sumWorkoutMap = let f sum amounts = sum + (foldl (+) 0 amounts)
                in M.foldl f 0

showSummary :: Int -> String
showSummary = ("Your workout summary is: " ++) . show

main = do
  args <- getArgs
  (inputPaths, optionFns) <- parseArgs args
  filterFn <- processOptions optionFns
  content <- readInputsStream inputPaths
  putStrLn (showSummary . sumWorkoutMap . filterWorkoutMap filterFn . parseInputs . convertInputsStream $ content) >> exit
