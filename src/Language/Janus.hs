module Language.Janus (
  someFunc
) where

import           Language.Janus.AST
import           Language.Janus.Interpreter
import           Language.Janus.Parser

someFunc :: IO ()
someFunc = putStrLn "someFunc"
