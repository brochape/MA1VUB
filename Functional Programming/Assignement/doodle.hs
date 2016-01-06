module Doodle (StringSet, Timestamp, Doodle(initialize, add, remove, toogle), Pool(freshKey, get, set), run) where
import qualified Data.List as L
import qualified Data.Map  as M
import qualified System.IO

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

class Doodle d where
  initialize :: String -> d t
  add :: Ord t => (t,t) -> d t -> d t
  remove :: Int -> d t -> d t
  toogle :: String -> Int -> d t -> d t

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

  toogle name num (StringSet titl tss) = let (ys,(from,to,names):zss) = splitAt num tss
                                         in StringSet titl (ys ++ (if name `elem` names then [(from,to,filter (\a -> a /= name) names)] else [(from,to,name:names)]) ++ zss)


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
populate d = putStrLn (show d) >> prompt "Add/Remove a slot?" (populate d) f
  where f Nothing                = return d
        f (Just (Left i))        = populate $ remove i d
        f (Just (Right (t1,t2))) = populate $ add (read t1, read t2) d

participate :: (Doodle d, Show (d t)) => String -> d t -> IO (d t)
participate n d = putStrLn (show d) >> prompt "Toogle a slot?" (participate n d) f
  where f Nothing  = putStrLn "Thanks for participating!" >> return d
        f (Just i) = participate n (toogle n i d)


