module Test.Imperial where

import Data.Metrology
import Data.Metrology.Z
import Data.Metrology.Factor
import Data.Metrology.Show
import Data.Metrology.SI as SI
import Data.Metrology.Imperial.Units

main :: IO ()
main = do
  putStrLn $ "1 mile is " ++ show (1 % Mile :: SI.Length) 
  putStrLn $ "1 pound is " ++ show (1 % Pound :: SI.Mass)   
  
