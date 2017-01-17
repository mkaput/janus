module Main where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IO.Class (liftIO)
import           Data.Char
import           System.IO

import           Language.Janus.AST
import           Language.Janus.Interp
import           Language.Janus.Parser

main :: IO ()
main = do
  putStrLn "Janus REPL"
  putStrLn "type :q to quit"
  escInterp prompt

escInterp :: InterpM a -> IO ()
escInterp m = do
  result <- runInterpM m
  case result of
    Left err -> putStrLn "ERROR" >> print err >> return ()
    _        -> return ()

prompt :: InterpM ()
prompt = do
  liftIO (putStr ">>> " >> hFlush stdout)
  line <- trim <$> liftIO getLine
  case line of
    "" -> prompt
    ":q" -> return ()
    (':':_) -> do
      liftIO (putStrLn "unknown meta command" >> hFlush stdout)
      prompt
    _ -> do
      result <- runLine line
      liftIO (print result >> hFlush stdout)
      prompt

  where
    runLine line = case parseStatement line of
      Left parseErr -> do
        liftIO (print parseErr >> hFlush stdout)
        return JUnit
      Right ast -> eval ast `catchError` \err -> do
        liftIO (print err >> hFlush stdout)
        return JUnit

trim xs = dropSpaceTail "" $ dropWhile isSpace xs
  where
    dropSpaceTail maybeStuff "" = ""
    dropSpaceTail maybeStuff (x:xs)
            | isSpace x = dropSpaceTail (x:maybeStuff) xs
            | null maybeStuff = x : dropSpaceTail "" xs
            | otherwise       = reverse maybeStuff ++ x : dropSpaceTail "" xs
