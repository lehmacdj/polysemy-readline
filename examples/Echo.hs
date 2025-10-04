{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Main where

import Polysemy
import Polysemy.Readline

repl :: Member Readline r => Sem r ()
repl = do
  mline <- getInputLine "> "
  case mline of
    Nothing -> pure ()
    Just line -> outputStrLn line >> repl

main :: IO ()
main =
  runInputT defaultSettings
    . runFinal
    . runReadlineFinal
    $ repl
