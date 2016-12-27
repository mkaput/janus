module Language.Janus (
  someFunc
) where

import           Language.Janus.AST
import           Language.Janus.Interp
import           Language.Janus.Parser

someFunc :: IO ()
someFunc = putStrLn "someFunc"
