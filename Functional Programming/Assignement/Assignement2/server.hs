module Main where

import qualified Doodle (sockHandler,ServerData(..))
import qualified Data.Map  as M
import qualified Network (listenOn, accept, PortID(..), Socket)
import qualified System.Environment (getArgs)
import qualified System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import qualified Control.Concurrent.STM as STM
import qualified System.Random as R

type UserDict = M.Map String String

-- data ServerData = ServerData { admin          :: (String,String)
--                              , students       :: (M.Map String String)
--                              , teachers       :: (M.Map String String)
--                              , doodles        :: [(String,Doodle.StringSet Doodle.Timestamp)]--[(teacher, slots and title)]
--                              , courses        :: (M.Map String [String])
--                              }


main :: IO ()
main =  do
            args <- System.Environment.getArgs
            let port = fromIntegral (read $ head args :: Int)
            let admin = (args !! 1) :: String
            let password = (args !! 2) :: String
            sock <- Network.listenOn $ Network.PortNumber port
            putStrLn $ "Listening on " ++ head args
            gen <- R.getStdGen
            let pwlist =  R.randomRs ('a','z') gen 
            sdata <- STM.atomically $ STM.newTVar (Doodle.ServerData (admin,password) (M.empty :: UserDict) (M.empty ::UserDict) [] (M.empty ::M.Map String [String]) pwlist)
            Doodle.sockHandler sock sdata
            return ()

-- data ServerData = ServerData { admin          :: (String,String)
--                              , students       :: M.Map String String
--                              , teachers       :: M.Map String String
--                              , doodles        :: [(String,StringSet Timestamp)]   --[(teacher, slots and title)]
--                              , courses  :: M.Map String [String]
--                              }