module Main where

import           Control.Monad         (void)

import           Options.Applicative   ((<>))
import qualified Options.Applicative   as O

import           Language.Janus.AST
import           Language.Janus.Interp
import           Language.Janus.Parser

data Arguments = Arguments String
               deriving (Show)

main :: IO ()
main = O.execParser opts >>= runApp
 where
  opts = O.info (O.helper <*> argdefs) O.fullDesc

  argdefs = Arguments
    <$> O.strArgument (O.metavar "FILE" <> O.help "source file")

runApp :: Arguments -> IO ()
runApp (Arguments filename) = do
  source <- readFile filename
  case parseProgram filename source of
    Left err  -> print err
    Right ast -> run ast >>= either print (\_ -> return ())
