module Main where

import qualified Doodle (sockHandler,ServerData(..))
import qualified Data.Map  as M
import qualified Network (listenOn, accept, PortID(..), Socket)
import qualified System.Environment (getArgs)
import qualified System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import qualified Control.Concurrent.STM as STM
import qualified System.Random as R

type UserDict = M.Map String String

main :: IO ()
main =  do
            [port,admin,password] <- System.Environment.getArgs
            sock <- Network.listenOn $ Network.PortNumber $ fromIntegral (read port::Int)
            putStrLn $ "Listening on " ++ show port
            gen <- R.getStdGen
            let pwlist =  R.randomRs ('a','z') gen 
            sdata <- STM.atomically $ STM.newTVar (Doodle.ServerData (admin,password) (M.empty :: UserDict) (M.empty ::UserDict) [] (M.empty ::M.Map String [String]) pwlist)
            Doodle.sockHandler sock sdata
            return ()
