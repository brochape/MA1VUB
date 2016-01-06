module Main where
import Doodle
import qualified Data.Map  as M

main :: IO ()
main = run (M.empty::(M.Map Int (StringSet Timestamp)))