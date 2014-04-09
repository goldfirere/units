module Test.Imperial where

import Data.Dimensions
import Data.Dimensions.Z
import Data.Dimensions.DimSpec
import Data.Dimensions.Show
import Data.Dimensions.SI as SI
import Data.Dimensions.Imperial.Units

main :: IO ()
main = do
  putStrLn $ "1 mile is " ++ show (1 % Mile :: SI.Length) 
  putStrLn $ "1 pound is " ++ show (1 % Pound :: SI.Mass)   
  
