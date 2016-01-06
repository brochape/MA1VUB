module Doodle (StringSet, Cmd, Timestamp(..), ServerData(..), Doodle(initialize, add, remove, toogle), Pool(freshKey, get, set), run,sockHandler) where
import qualified Control.Concurrent     (forkIO)
import qualified Control.Concurrent.STM as STM
import qualified Control.Monad.STM      as MSTM
import qualified Data.List              as L
import qualified Data.Map               as M
import qualified Data.Maybe             as D
import qualified Network                (Socket, accept)
import qualified Parser
import qualified System.IO
import qualified System.Random          as R


prompt :: Read a => String -> IO b -> (a -> IO b) -> IO b
prompt s m f = do putStrLn s
                  xs <- fmap (readsPrec 0) System.IO.getLine
                  if null xs
                     then System.IO.putStrLn "What???" >> m
                     else f $ fst $ head xs

data Timestamp = Timestamp { year      :: Int
                           , month     :: Int
                           , day       :: Int
                           , hour      :: Int
                           , mins      :: Int
                           , timezoneH :: Int
                           , timezoneM :: Int
                           }

data StringSet t = StringSet { title     :: String
                             , timeslots :: [(t,t,[String])]
                             } deriving (Ord,Eq,Read)


data ServerData = ServerData { admin    :: (String,String)
                             , students :: M.Map String String
                             , teachers :: M.Map String String
                             , doodles  :: [(String,StringSet Timestamp)]--[(teacher, slots and title)]
                             , courses  :: M.Map String [String]
                             , passList :: String
                             }


class Doodle d where
  initialize :: String -> d t
  add :: Ord t => (t,t) -> d t -> d t
  remove :: Int -> d t -> d t
  toogle :: String -> Maybe Int -> d t -> d t

class Pool p where
  freshKey :: (Ord k, Enum k) => p k (d t) -> k
  get :: Ord k => k -> p k (d t) -> Maybe (d t)
  set :: Ord k => k -> (d t) -> p k (d t) -> p k (d t)

instance Read Timestamp where
  readsPrec _ = parseDate

instance Show t => Show (StringSet t) where
  show (StringSet titl tss)= let maxSize = maximum $ map (stringsSize .(\(_,_,c)->c)) tss ++ [length titl] -- the maximum size taken by the text
                                 titleSize = maxSize - length titl
                                 dashes = concat $ replicate maxSize "-" in
                              "+" ++ dashes ++ "+\n" ++
                              "|" ++ titl ++ concat ( replicate titleSize " " )++ "|" ++
                              "\n+"++ dashes ++ "+\n" ++
                              L.intercalate "\n" (map (printSlot maxSize) tss ) ++
                              if null tss then "" else "\n+"++dashes ++ "+\n"
instance Show Timestamp where
  show = dateToString

instance Eq Timestamp where
  x == y = let Timestamp y1 mo1 d1 h1 mi1 _ _ = timeStampToUTC x
               Timestamp y2 mo2 d2 h2 mi2 _ _ =  timeStampToUTC y
           in y1 == y2 &&
              mo1 == mo2 && d1 == d2 &&
              h1 == h2 && mi1 == mi2

instance Ord Timestamp where
  compare x y = let Timestamp y1 mo1 d1 h1 mi1 _ _ = timeStampToUTC x
                    Timestamp y2 mo2 d2 h2 mi2 _ _ =  timeStampToUTC y
                in compare (y1, mo1, d1, h1, mi1) (y2, mo2 ,d2, h2, mi2)

instance Doodle StringSet where -- StringSet is here formed by a title and a list of tuples containing a start timestamp, an end timestamp and a list of strings (persons who toggled that slot)
  initialize str = StringSet str []
  add (from, to) (StringSet titl tss) = if any (areEqualSlots (from,to)) tss || from >= to || any (areOverlappingSlots (from,to)) tss
                                        then StringSet titl tss
                                        else StringSet titl ((from, to, []):tss)
                                        where areEqualSlots (x,y) (x2,y2,_)         = (x2,y2) == (x,y)
                                              areOverlappingSlots (x1,y1) (x2,y2,_) = (x2 >= x1 && x2 < y1) || (y2 > x1 && y2 <= y1)


  remove num (StringSet titl tss) = let (ys,zs) = splitAt num tss
                                    in StringSet titl (ys++tail zs)

  toogle name num (StringSet titl tss) = let (ys,(from,to,names):zss) = splitAt2 num tss
                                         in StringSet titl (ys ++ (if name `elem` names then [(from,to,filter (/= name) names)] else [(from,to,name:names)]) ++ zss)


instance Pool M.Map where
  freshKey pool = if M.null pool then toEnum 1 else succ $ fst $ M.findMax pool
  get = M.lookup
  set = M.insert

stringsSize :: [String] -> Int -- Calculates the total length of strings
stringsSize lst = 46 + length  (concat lst ) + length lst

parseDate :: String -> [(Timestamp,String)] -- Creating a Timeslot from a String
parseDate str = let (yr, rest) = splitAt 4 str
                    (mnth, rest1) = splitAt 2 $ tail rest
                    (d, rest2) = splitAt 2 $ tail rest1
                    (hr, rest3) = splitAt 2 $ tail rest2
                    (mns, rest4) = splitAt 2 $ tail rest3
                    (sign:tzhour, rest5) = splitAt 3 rest4
                    (tzmin, restfinal) = splitAt 2 $ tail rest5
                in [(Timestamp (read yr) (read mnth) (read d) (read hr) (read mns) (if sign == '-' then -(read tzhour) else read tzhour) (read tzmin), restfinal)]

timeStampToUTC :: Timestamp -> Timestamp -- Converts a timestamp to the UTC timezone
timeStampToUTC (Timestamp y mo d h m tzh tzm) = Timestamp y' mo'' d'' (h' `mod` 24) (m' `mod` 60) 0 0
    where m' = m + tzm
          h' = h + tzh + (if m' > 59 then 1 else if m < 0 then (-1) else 0)
          d' = d + (if h' < 0 then -1 else if h' > 23 then 1 else 0)
          d'' | d' < 1              = daysInMonth (mo-1)
              | d' > daysInMonth mo = 1
              | otherwise           = d'
          mo' | d' < 1              = mo - 1
              | d' > daysInMonth mo = mo + 1
              | otherwise           = mo
          mo'' | mo' == 13          = 1
               | mo' == 0           = 12
               | otherwise          = mo'
          y' | mo' == 0             = y - 1
             | mo' == 13            = y + 1
             | otherwise            = y
          daysInMonth n | n == 2    = if isLeapYear then 29 else 28
                        | n `elem` [4, 6, 9, 11] = 30
                        | otherwise              = 31
          isLeapYear = if y `mod` 100 == 0 then y `mod` 400 == 0 else y `mod` 4 == 0


dateToString :: Timestamp -> String -- Converts a Timestam to a String
dateToString tst = show (year tst) ++ "-" ++ fillSmallNum(show (month tst))++
                   "-" ++ fillSmallNum(show (day tst))++ "T" ++ fillSmallNum (show (hour tst)) ++
                   ":" ++  fillSmallNum ( show (mins tst)) ++ (if timezoneH tst > 0 then "+" else "-") ++ fillSmallNum(show ( abs $ timezoneH tst)) ++
                   ":" ++ fillSmallNum(show (timezoneM tst))

fillSmallNum :: String -> String -- Adds a 0 in front of numbers <10 for print purpose
fillSmallNum cs = if (read cs::Int) < (10::Int) then "0" ++ cs else cs

printSlot:: Show t => Int -> (t, t, [String]) -> String -- Pretty print for slots
printSlot maxSize (from,to,cs) = "|" ++ show from ++ "|" ++ show to ++ "|" ++
                                 L.intercalate "," cs ++
                                 concat ( replicate (maxSize - 46 - length (concat cs) - max (length cs - 1) 0) " ") ++
                                 "|"

run :: (Read t, Doodle d, Show k, Ord k, Enum k, Read k, Pool p, Show (d t), Ord t) => p k (d t) -> IO ()
run p = prompt "Create a new doodle or participate to an existing one?" (return p) (turn p) >>= run

turn :: (Read t, Doodle d, Ord k, Show k, Enum k, Read k, Pool p, Show (d t), Ord t) => p k (d t) -> Either String k -> IO (p k (d t))
turn p (Left s)  = do d <- (populate $ initialize s)
                      let k = freshKey p
                      putStrLn $ "Doodle ID: " ++ show k
                      return $ set k d p
turn p (Right k) = maybe (System.IO.putStrLn "Unknown doodle" >> return p)
                         (\d1 -> prompt "What is your name?"
                                        (turn p (Right k))
                                        (\s -> fmap (\d -> (set k d p)) (participate s d1)))
                         (get k p)

populate :: (Read t, Doodle d, Show (d t), Ord t) => d t -> IO (d t)
populate d = print d >> prompt "Add/Remove a slot?" (populate d) f
  where f Nothing                = return d
        f (Just (Left i))        = populate $ remove i d
        f (Just (Right (t1,t2))) = populate $ add (read t1, read t2) d

participate :: (Doodle d, Show (d t)) => String -> d t -> IO (d t)
participate n d = print d >> prompt "Toogle a slot?" (participate n d) f
  where f Nothing  = putStrLn "Thanks for participating!" >> return d
        f (Just i) = participate n (toogle n i d)


-----------------------------PART 2------------------------


sockHandler :: Network.Socket -> STM.TVar ServerData -> IO ServerData
sockHandler sock sdata= do
    (handle, _, _) <- Network.accept sock
    System.IO.hSetBuffering handle System.IO.NoBuffering -- Avoids unnecessary buffering
    putStrLn "Received connexion"
    _ <- Control.Concurrent.forkIO $ commandProcessor handle sdata
    sockHandler sock sdata

commandProcessor :: System.IO.Handle -> STM.TVar ServerData -> IO ()
commandProcessor handle sdata = do
                                  line <- System.IO.hGetLine handle
                                  putStrLn "Received request"
                                  [(cmd,retCode)] <- return $ Parser.apply parse line
                                  putStrLn $ show cmd ++ show retCode
                                  answer <- runCommand cmd sdata
                                  System.IO.hPutStrLn handle $ answerToString answer
                                  System.IO.hClose handle

runCommand :: Cmd -> STM.TVar ServerData -> IO Answer
runCommand cmd sdata = MSTM.atomically $ do servdata <- STM.readTVar sdata
                                            updateServerData sdata $ exec cmd servdata

updateServerData :: STM.TVar ServerData -> (ServerData,Answer) -> MSTM.STM Answer
updateServerData sharedSd (sd,ans) = do STM.writeTVar sharedSd sd
                                        return ans
data Cmd = AddTeacher String String String
         | AddStudent String String String
         | ChangePassword String String String
         | GetDoodle String String String
         | SetDoodle String String String [(Timestamp,Timestamp)] --(Doodle StringSet)
         | Subscribe String String String
         | Prefer String String String (Timestamp,Timestamp)
         | ExamSchedule
         | ParseErr
         deriving Show

exec :: Cmd -> ServerData -> (ServerData, Answer)
exec (AddTeacher login password teacherName) sdata =
      if checkLoginAdmin (login, password) sdata then addTeacher teacherName sdata else (sdata, WrongLogin)

exec (AddStudent login password studentName) sdata =
      if checkLoginAdmin (login, password) sdata then addStudent studentName sdata else (sdata, WrongLogin)

exec (ChangePassword login password newPassword) sdata = changePassword (login,password) newPassword sdata

exec (GetDoodle login password doodleName) sdata@(ServerData admn studnts teachrs dodles crss pwlist) =
      if checkLogin (login, password) teachrs || checkLogin (login, password) studnts
      then getDoodle doodleName sdata
      else (sdata, WrongLogin)

exec (SetDoodle login password doodleName doodle) sdata@(ServerData admn studnts teachrs dodles crss pwlist) =
      if checkLogin (login, password) teachrs then setDoodle login doodleName doodle sdata else (sdata, WrongLogin)

exec (Subscribe login password doodleName) sdata@(ServerData admn studnts teachrs dodles crss pwlist) =
      if checkLogin (login, password) studnts then subscribe login doodleName sdata else (sdata, WrongLogin)

exec (Prefer login password doodleName (from,to)) sdata@(ServerData admn studnts teachrs dodles crss pwlist) =
      if checkLogin (login, password) studnts then prefer login doodleName (from, to) sdata else (sdata, WrongLogin)

exec ParseErr sdata = (sdata, ParseError)
     


answerToString:: Answer -> String
answerToString WrongLogin = "wrong-login"
answerToString (OkToken token) = "ok" ++ show token
answerToString (OkDoodle doodle) = "ok" ++ show doodle
answerToString (OkSchedule schedule) = "ok" ++ show schedule
answerToString OK = "ok"
answerToString IdTaken = "id-taken"
answerToString NoId = "no-such-id"
answerToString NoSlot = "no-such-slot"
answerToString NotSub = "not-subscribed"
answerToString NoSchedule = "no-possible-exam-schedule"
answerToString ParseError = "parse-error"

parse :: Parser.Parser Cmd
parse = Parser.oneof[parseAddTeacher,parseAddStudent,parseChangePassword,parseGetDoodle,parseSetDoodle,parseSubscribe,parsePrefer,parseExamSchedule,parseError]

parseAddTeacher :: Parser.Parser Cmd
parseAddTeacher = do Parser.keyword "add-teacher"
                     login <- Parser.token
                     Parser.char '@'
                     password <- Parser.token2
                     token <- Parser.token
                     return $ AddTeacher login password token

parseAddStudent :: Parser.Parser Cmd
parseAddStudent = do Parser.keyword "add-student"
                     login <- Parser.token
                     Parser.char '@'
                     password <- Parser.token2
                     token <- Parser.token
                     return $ AddStudent login password token

parseChangePassword :: Parser.Parser Cmd
parseChangePassword = do Parser.keyword "change-password"
                         login <- Parser.token
                         Parser.char '@'
                         password <- Parser.token2
                         token <- Parser.token
                         return $ ChangePassword login password token

parseGetDoodle :: Parser.Parser Cmd
parseGetDoodle = do Parser.keyword "get-doodle"
                    login <- Parser.token
                    Parser.char '@'
                    password <- Parser.token2
                    token <- Parser.token
                    return $ GetDoodle login password token

parseSetDoodle :: Parser.Parser Cmd
parseSetDoodle = do Parser.keyword "set-doodle"
                    login <- Parser.token
                    Parser.char '@'
                    password <- Parser.token2
                    doodleName <- Parser.token
                    Parser.blank
                    doodle <- parseSlots
                    return $ SetDoodle login password doodleName doodle-- TODO!!!!

parseSubscribe :: Parser.Parser Cmd
parseSubscribe = do Parser.keyword "subscribe"
                    login <- Parser.token
                    Parser.char '@'
                    password <- Parser.token2
                    token <- Parser.token
                    return $ Subscribe login password token

parsePrefer :: Parser.Parser Cmd
parsePrefer = do Parser.keyword "prefer"
                 login <- Parser.token
                 Parser.keyword "@"
                 password <- Parser.token2
                 token <- Parser.token
                 timestamp <- parseSlot
                 return $ Prefer login password token timestamp

parseError :: Parser.Parser Cmd
parseError = return ParseErr


some :: Parser.Parser a -> Parser.Parser [a]
some p = do x <- p
            xs <- many p
            return $ x:xs

many :: Parser.Parser a -> Parser.Parser [a]
many p = Parser.orelse
           (do Parser.char ','
               x <- p
               xs <- many p
               return $ x:xs)
           (return [])

parseSlots :: Parser.Parser [(Timestamp,Timestamp)]
parseSlots = do Parser.string "["
                ts <- some parseSlot
                Parser.string "]"
                return ts


parseSlot :: Parser.Parser (Timestamp,Timestamp)
parseSlot = do from <- parseTime
               Parser.keyword "/"
               Parser.blank
               to <- parseTime
               return (from,to)

parseTime :: Parser.Parser Timestamp
parseTime = do year <- Parser.integer
               Parser.char '-'
               month <- Parser.integer
               Parser.char '-'
               day <- Parser.integer
               Parser.char 'T'
               hour <- Parser.integer
               Parser.char ':'
               minutes <- Parser.integer
               Parser.char '+' --`Parser.orelse` Parser.char '-'
               tzh <- Parser.integer
               Parser.char ':'
               tzm <- Parser.integer
               return $ Timestamp year month day hour minutes tzh tzm


parseExamSchedule :: Parser.Parser Cmd
parseExamSchedule = do Parser.keyword "exam-schedule"
                       return ExamSchedule

echoCommand :: System.IO.Handle -> [String] -> IO ()
echoCommand handle cmd = System.IO.hPutStrLn handle (unwords $ tail cmd)


data Answer = WrongLogin
            | OkToken String
            | OkDoodle [(Timestamp,Timestamp)]
            | OkSchedule (M.Map String (Timestamp,Timestamp))
            | OK
            | IdTaken
            | NoId
            | NoSlot
            | NotSub
            | NoSchedule
            | ParseError
            deriving Show


generatePassword :: String -> (String,String)
generatePassword = splitAt 4

addTeacher :: String -> ServerData -> (ServerData, Answer)
addTeacher cs (ServerData admn studnts teachrs dodles crss pwlist) =  let (password,newpwlist) = generatePassword pwlist
                                                                      in
                                                                        if D.isNothing $ M.lookup cs teachrs
                                                                        then (ServerData admn studnts (M.insert cs password teachrs) dodles crss newpwlist, OkToken password)
                                                                        else (ServerData admn studnts teachrs dodles crss pwlist, IdTaken)

addStudent :: String -> ServerData -> (ServerData, Answer)
addStudent cs (ServerData admn studnts teachrs dodles crss pwlist) = let (password,newpwlist) = generatePassword pwlist
                                                                     in
                                                                      if D.isNothing $ M.lookup cs studnts
                                                                      then (ServerData admn (M.insert cs password studnts) teachrs dodles crss newpwlist,OkToken password)
                                                                      else (ServerData admn studnts teachrs dodles crss pwlist, IdTaken)

changePassword :: (String,String) -> String -> ServerData -> (ServerData,Answer)
changePassword (name, password) newPassword (ServerData admn studnts teachrs dodles crss pwlist)
    | M.lookup name studnts == Just password =
      (ServerData admn(M.update (\ x -> if x == password then Just newPassword else Just x) name studnts) teachrs dodles crss pwlist, OK)
    | M.lookup name teachrs == Just password =
      (ServerData admn studnts (M.update (\ x -> if x == password then Just newPassword else Just x) name teachrs) dodles crss pwlist, OK)
    | otherwise =
      (ServerData admn studnts teachrs dodles crss pwlist, WrongLogin)


setDoodle :: String -> String -> [(Timestamp,Timestamp)] -> ServerData -> (ServerData, Answer)
setDoodle teacher titl doodle (ServerData admn studnts teachrs dodles crss pwlist) =
                                                                  if any (\(teach,StringSet tit _) -> tit == titl && teacher == teach) dodles
                                                                  then (ServerData admn studnts teachrs dodles crss pwlist, IdTaken)
                                                                  else (ServerData admn studnts teachrs ((teacher, StringSet titl (map (\(a,b) -> (a,b,[])) doodle)):dodles) (M.insert titl [] crss ) pwlist, OK)

getDoodle :: String -> ServerData -> (ServerData, Answer) -- [(String,StringSet Timestamp)]
getDoodle doodleName sdata@(ServerData admn studnts teachrs dodles crss pwlist) = let doodle = filter (\a -> title a == doodleName) $ map snd dodles
                                                                                      slotsPers = (\[StringSet _ tslots] -> tslots) doodle
                                                                                      retDoodle = map (\(a,b,_) -> (a,b)) slotsPers
                                                                                  in
                                                                                     if null doodle
                                                                                     then (sdata,NoId)
                                                                                     else (sdata, OkDoodle retDoodle)


subscribe :: String -> String -> ServerData -> (ServerData,Answer)
subscribe login courseName (ServerData admn studnts teachrs dodles crss pwlist) =
                                                                 if D.isNothing $ M.lookup courseName crss
                                                                 then (ServerData admn studnts teachrs dodles crss pwlist, NoId)
                                                                 else (ServerData admn studnts teachrs dodles (M.insertWith (++) courseName [login] crss ) pwlist,OK)

prefer :: String -> String -> (Timestamp, Timestamp) -> ServerData ->  (ServerData,Answer)
prefer studentName doodleName (from,to) sdata@(ServerData admn studnts teachrs dodles crss pwlist) = let index = L.findIndex (\(_,StringSet x _) -> x == doodleName) dodles
                                                                                                         (mostdoodles,(teacher,doodle@(StringSet title slots)):xs) = splitAt2 index dodles
                                                                                                         index2 = L.findIndex (\(a,b,_) -> (a,b) == (from,to) ) slots
                                                                                                         correctDoodle = toogle studentName index2 doodle
                                                                                                         correctDoodles = mostdoodles ++ [(teacher,correctDoodle)] ++ xs

                                                                                                     in
                                                                                                      if D.isNothing index
                                                                                                      then (sdata, NoId)
                                                                                                      else (ServerData admn studnts teachrs correctDoodles crss pwlist, OK)

splitAt2 :: Maybe Int -> [a] -> ([a],[a])
splitAt2 Nothing _ = ([],[])
splitAt2 (Just index) lst = splitAt index lst

-- examSchedule ::
checkLoginAdmin :: (String, String) -> ServerData -> Bool
checkLoginAdmin (iD, pw) (ServerData admn studnts teachrs dodles crss pwlist) = (iD,pw) == admn

checkLogin :: (String, String) -> M.Map String String -> Bool
checkLogin (iD,pw) loginsMap = M.lookup iD loginsMap == Just pw
