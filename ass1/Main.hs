import ParseLib.Abstract
import System.Environment
import System.IO

import Data.Char

import Prelude hiding ((<$>), (<$), (<*>), (<*), (*>), sequence)

-- Starting Framework

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc :: Bool }
  deriving (Eq, Ord)

data Date = Date { year  :: Year
                 , month :: Month
                 , day   :: Day }
  deriving (Eq, Ord)

newtype Year  = Year { unYear :: Int }  deriving (Eq, Ord)
newtype Month = Month { unMonth :: Int } deriving (Eq, Ord)
newtype Day   = Day { unDay :: Int } deriving (Eq, Ord)

data Time = Time { hour   :: Hour
                 , minute :: Minute
                 , second :: Second }
  deriving (Eq, Ord)

newtype Hour   = Hour { unHour :: Int } deriving (Eq, Ord)
newtype Minute = Minute { unMinute :: Int } deriving (Eq, Ord)
newtype Second = Second { unSecond :: Int } deriving (Eq, Ord)

-- | The main interaction function. Used for IO, do not edit.
data Result = SyntaxError | Invalid DateTime | Valid DateTime deriving (Eq, Ord)

instance Show DateTime where
  show = printDateTime

instance Show Result where
  show SyntaxError = "date/time with wrong syntax"
  show (Invalid _) = "good syntax, but invalid date or time values"
  show (Valid x)   = "valid date: " ++ show x

main :: IO ()
main = mainDateTime

mainDateTime :: IO ()
mainDateTime = interact (printOutput . processCheck . processInput)
  where
    processInput = map (run parseDateTime) . lines
    processCheck = map (maybe SyntaxError (\x -> if checkDateTime x then Valid x else Invalid x))
    printOutput  = unlines . map show

mainCalendar :: IO ()
mainCalendar = do
  file:_ <- getArgs
  res    <- readCalendar file
  putStrLn $ maybe "Calendar parsing error" (ppMonth (Year 2012) (Month 11)) res

-- Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <* parseTimeSep <*> parseTime <*> parseUTC

-- Date parsers
parseDate :: Parser Char Date
parseDate  = Date  <$> parseYear <*> parseMonth <*> parseDay
parseYear :: Parser Char Year
parseYear  = Year  <$> parseNInteger 4
parseMonth :: Parser Char Month
parseMonth = Month <$> parseNInteger 2
parseDay :: Parser Char Day
parseDay   = Day   <$> parseNInteger 2

-- Sep parser
parseTimeSep :: Parser Char Char
parseTimeSep = symbol 'T'

-- Time parsers
parseTime :: Parser Char Time
parseTime = Time   <$> parseHour <*> parseMin <*> parseSec
parseHour :: Parser Char Hour
parseHour = Hour   <$> parseNInteger 2
parseMin :: Parser Char Minute
parseMin  = Minute <$> parseNInteger 2
parseSec :: Parser Char Second
parseSec  = Second <$> parseNInteger 2

--UTC parser
parseUTC :: Parser Char Bool
parseUTC = parseCharToBool 'Z'

-- Helper parsers, e.g., "00151" -> 151.
parseNInteger :: Int -> Parser Char Int
parseNInteger n = foldl (\a b -> a * 10 + b) 0 <$> sequence (replicate n newdigit)

parseCharToBool :: Char -> Parser Char Bool
parseCharToBool c = True <$ symbol c <|> False <$ epsilon 

-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run p s = run' $ parse p s
  where
    run' :: [(a, [s])] -> Maybe a
    run' []          = Nothing
    run' ((a, []):_) = Just a
    run' (_:s)       = run' s

-- Exercise 3
printDateTime :: DateTime -> String
printDateTime (DateTime d t u) = printDate d ++ "T" ++ printTime t ++ printUTC
  where
    printDate :: Date -> String
    printDate (Date year month day)     = concat $ map fillLeftShow [(unYear year, 4), (unMonth month, 2)  , (unDay day, 2)]

    printTime :: Time -> String
    printTime (Time hour minute second) = concat $ map fillLeftShow [(unHour hour, 2), (unMinute minute, 2), (unSecond second, 2)]

    printUTC :: String
    printUTC
      | u         = ['Z']
      | otherwise = []

    -- Converts an Integer to a String while prepending n zero's to the output.
    fillLeftShow :: (Int, Int) -> [Char]
    fillLeftShow (x, n) = replicate (n - length xstr) '0' ++ xstr
      where xstr = show x

-- Exercise 4
parsePrint s = fmap printDateTime $ run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime (DateTime (Date y m d) (Time h min s) _) =
  checkYear   y     &&
  checkMonth  m     &&
  checkDay    y m d &&
  checkHour   h     &&
  checkMinute min   &&
  checkSecond s

checkDay :: Year -> Month -> Day -> Bool
checkDay y (Month m) (Day x)
  | x < 0 || x > 31        = False
  | elem m largestMonths   = x <= 31
  | m == 2 && isLeapYear y = x <= 29
  | m == 2                 = x <= 28
  | otherwise              = x <= 30

-- All months with 31 days.
largestMonths :: [Int]
largestMonths = [1, 3, 5, 7, 8, 10, 12]

isLeapYear :: Year -> Bool
isLeapYear (Year y)
  | y `mod` 400 == 0 = True
  | y `mod` 100 == 0 = False
  | y `mod` 4   == 0 = True
  | otherwise        = False

checkBetween :: Ord a => a -> a -> a -> Bool
checkBetween n minn maxn = (n >= minn) && (n <= maxn)

checkYear :: Year -> Bool
checkYear (Year x) = checkBetween x 0 9999
      
checkMonth :: Month -> Bool
checkMonth (Month x) = checkBetween x 1 12

checkHour :: Hour -> Bool
checkHour (Hour x) = checkBetween x 0 23

checkMinute :: Minute -> Bool
checkMinute (Minute x) = checkBetween x 0 59

checkSecond :: Second -> Bool
checkSecond (Second x) = checkBetween x 0 59

-- Exercise 6
data Calendar = Calendar { prodid  :: String
                         , version :: String
                          , events  :: [Event]
                         } deriving (Eq, Ord, Show)

data Event = Event { dtstart     :: DateTime
                   , dtend       :: DateTime
                   , location    :: Maybe String
                   , uid         :: String
                   , dtstamp     :: DateTime
                   , summary     :: Maybe String
                   , description :: Maybe String
                   } deriving (Eq, Ord, Show)

-- Exercise 7
data Token = Identifier String | Literal String deriving (Show)

scanCalendar :: Parser Char [Token]
scanCalendar = many scanCalendar'
  where
    scanCalendar' :: Parser Char Token
    scanCalendar' = symbol ' ' *> scanCalendar'
                <|> Literal    <$> (spaces *> symbol ':' *> some (satisfy (\x -> x /= '\n'))) <* token "\r\n"
                <|> Identifier <$> identifier

    spaces :: Parser Char String
    spaces = greedy (symbol ' ')

parseCalendar :: Parser Token Calendar
parseCalendar = Calendar 
            <$> (idIsStr "BEGIN"        *> litIsStr "VCALENDAR"
             *> parseLitById "PRODID") <*> parseLitById "VERSION" <*> parseEvents
            <*  idIsStr "END"          <*  litIsStr "VCALENDAR"
  where
    parseEvents :: Parser Token [Event]
    parseEvents = some (idIsStr "BEGIN"  *> litIsStr "VEVENT" *> parseEvent <* idIsStr "END" <* litIsStr "VEVENT")

    parseEvent :: Parser Token Event
    parseEvent = Event <$> tParseDateTime
                       <*> tParseDateTime
                       <*> parseOptional "LOCATION"
                       <*> parseLitById "UID"
                       <*> tParseDateTime
                       <*> parseOptional "SUMMARY"
                       <*> parseOptional "DESCRIPTION"

-- Parses a single token using a parser that is of type Parser Char r, where r is arbitrary.
parseTokenUsing :: Parser Char r -> Token -> r
parseTokenUsing ps (Literal s) = fst (head (parse ps s))

-- Parser an optional Literal relative to the Identifier based on an input string.
parseOptional :: String -> Parser Token (Maybe String)
parseOptional s = optional ((\(Literal x) -> x) <$> (idIsStr s *> parseMultiLiteral))

-- Parses a multi-line Literal based on the value in the Identifier.
parseLitById :: String -> Parser Token String
parseLitById s = (\(Literal x) -> x) <$> (idIsStr s *> parseMultiLiteral)

-- Parses a multi-line string to the DateTime type.
tParseDateTime :: Parser Token DateTime
tParseDateTime = anyIdentifier *> ((parseTokenUsing parseDateTime) <$> parseMultiLiteral)

-- Parses a multi-line literal.
parseMultiLiteral :: Parser Token Token
parseMultiLiteral = mergeLiterals <$> many anyLiteral
  where
    -- Merges two literals into one.
    mergeLiterals :: [Token] -> Token
    mergeLiterals xs = foldl1 (\(Literal x) (Literal y) -> Literal (x ++ "\r\n" ++ y)) xs

-- Parser which only accepts when a given string is the same as the string in an Identifier.
idIsStr :: String -> Parser Token Token
idIsStr s = satisfy isStr
  where 
    isStr (Identifier x) = s == x
    isStr _              = False

-- Parser which only accepts when a given string is the same as the string in a Literal.
litIsStr :: String -> Parser Token Token
litIsStr s = satisfy isStr
  where
    isStr (Literal x) = s == x
    isStr _           = False

-- Parser which accepts any Token.
anyToken :: Parser Token Token
anyToken = anyIdentifier <|> anyLiteral

-- Parser which accepts any Identifier.
anyIdentifier :: Parser Token Token
anyIdentifier = satisfy isIdentifier
  where
    isIdentifier :: Token -> Bool
    isIdentifier (Identifier _) = True
    isIdentifier _              = False

-- Parser which accepts any Literal.
anyLiteral :: Parser Token Token
anyLiteral = satisfy isLiteral
  where
    isLiteral :: Token -> Bool
    isLiteral (Literal _) = True
    isLiteral _           = False

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- Exercise 8
readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar fp = do x <- openFile fp ReadMode
                     hSetNewlineMode x noNewlineTranslation
                     y <- hGetContents x
                     return $ recognizeCalendar y

-- Exercise 9
-- DO NOT use a derived Show instance. Your printing style needs to be nicer than that :)
printCalendar :: Calendar -> String
printCalendar (Calendar version prodid events) = "BEGIN:VCALENDAR\r\n" 
                                              ++ "PRODID:"  ++ prodid ++ "\r\n" 
                                              ++ "VERSION:" ++ version ++ "\r\n" 
                                              ++ getEvents events 
                                              ++ "END:VCALENDAR"
  where
    getEvents :: [Event] -> String
    getEvents []     = ""
    getEvents events = foldl1 (++) (map makeEventString events)
      where 
        makeEventString :: Event -> String
        makeEventString (Event dtstart' dtend' location' uid' dtstamp' summary' description')
          = "BEGIN:EVENT\r\n" ++ foldl1 (++) [ line' ("DTSTART"    ,dtstart'    ,printDateTime)
                                             , line' ("DTEND"      ,dtend'      ,printDateTime)
                                             , line' ("LOCATION"   ,location'   ,maybeStr'    )
                                             , line' ("UID"        ,uid'        ,id           )
                                             , line' ("DTSTAMP"    ,dtstamp'    ,printDateTime)
                                             , line' ("SUMMARY"    ,summary'    ,maybeStr'    )
                                             , line' ("DESCRIPTION",description',maybeStr'    ) ] ++ "END:EVENT\r\n"
          where
            line' :: (String, a, (a -> String)) -> String
            line' (ide, lit, f)
              | f lit == "" = ""
              | otherwise   = ide ++ ":" ++ f lit ++ "\r\n"
                              
            maybeStr' :: Maybe String -> String
            maybeStr' Nothing  = ""
            maybeStr' (Just x) = x

-- Exercise 10
countEvents :: Calendar -> Int
countEvents (Calendar _ _ events) = length events

findEvents :: DateTime -> Calendar -> [Event]
findEvents dt (Calendar _ _ events) = findOverlap dt events

findOverlap :: DateTime -> [Event] -> [Event]
findOverlap dt events = filter (\x -> (dt >= dtstart x) && (dt < dtend x)) events

checkOverlapping :: Calendar -> Bool
checkOverlapping (Calendar _ _ events) = check events
  where 
    check :: [Event] -> Bool
    check [] = False
    check (event:events)
      |    (length (findOverlap (dtend event)   events)) == 0 
        && (length (findOverlap (dtstart event) events)) == 0 = check events
      | otherwise = True

timeSpent :: String -> Calendar -> Int
timeSpent s (Calendar _ _ events) = sumMinutes (map (\x -> dtDiff (dtstart x) (dtend x)) getEvents)
  where
    -- Gets the event of which the summary is the same.
    getEvents :: [Event]
    getEvents = filter (\x -> check' (summary x) s) events
      where
        check' :: Maybe String -> String -> Bool
        check' Nothing _  = False
        check' (Just x) s = x == s

    -- Generates the number of days and the number of seconds between two events.
    dtDiff :: DateTime -> DateTime -> (Day, Second)
    dtDiff dt1@(DateTime d1 t1 _) 
           dt2@(DateTime d2 t2 _) = (dateDiffDays d1 d2, timeDiff t1 t2)
      where
        -- Returns the time difference in Seconds to maintain precision.
        timeDiff :: Time -> Time -> Second
        timeDiff (Time (Hour h1) (Minute m1) (Second s1))
                (Time (Hour h2) (Minute m2) (Second s2)) = Second $ (s2 + 60 * (m2 + 60 * h2)) - (s1 + 60 * (m1 + 60 * h1))
      
        -- Calculates the number of days between two dates.
        dateDiffDays :: Date -> Date -> Day
        dateDiffDays (Date y1'@(Year y1) (Month m1) (Day d1))
                    (Date y2'@(Year y2) (Month m2) (Day d2))
                    = Day $ abs (yearDays y2' - yearDays y1') + sumMonths (y1', Month m1) (y2', Month m2) + (d2 - d1)
          where
            -- Number of days in a year.
            yearDays :: Year -> Int
            yearDays y
              | isLeapYear y = 366
              | otherwise    = 365

            -- Number of days in the months between two different dates.
            sumMonths :: (Year, Month) -> (Year, Month) -> Int
            sumMonths start@(y@(Year ystart), m@(Month mstart)) end@(yend, mend)
              | y > yend      = 0
              | start == end  = 0
              | mstart == 12  = (monthDays m) + sumMonths (Year (ystart + 1), Month 1) end
              | otherwise     = (monthDays m) + sumMonths (y, Month (mstart + 1)) end
              where
                monthDays :: Month -> Int
                monthDays (Month m)
                  | elem m largestMonths   = 31
                  | m == 2 && isLeapYear y = 29
                  | m == 2                 = 28
                  | otherwise              = 30

    -- Calculates the number of minutes using a number of days and seconds.
    sumMinutes :: [(Day, Second)] -> Int
    sumMinutes [] = 0
    sumMinutes ((Day d, Second s):xs) = (d * 1440 + s `div` 60) + sumMinutes xs

-- Exercise 11
ppMonth :: Year -> Month -> Calendar -> String
ppMonth y m c = undefined
