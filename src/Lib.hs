module Lib
    ( someFunc
    ) where
import Data.Text.Lazy.IO
import Data.Text.Lazy as LT

someFunc :: IO ()
someFunc = putStrLn "someFunc"
