{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | This libraries provides a polysemy effect that provides interactive command
-- line usage.
module Polysemy.Readline
  ( -- * Effect and Actions
    Readline (..),
    getInputLine,
    getInputLineWithInitial,
    getInputChar,
    getPassword,
    waitForAnyKey,
    outputStr,
    outputStrLn,

    -- * Interpreters
    runReadline,
    runReadlineFinal,
    interpretReadlineAsInputT,

    -- * Re-exports from @haskeline@
    H.Settings (..),
    H.defaultSettings,
    H.runInputT,
  )
where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Polysemy
import Polysemy.Embed
import qualified System.Console.Haskeline as H

-- | For documentation on actions see haskeline's functions with the same name
-- and similar type signatures.
data Readline (m :: * -> *) a where
  GetInputLine :: String -> Readline m (Maybe String)
  GetInputLineWithInitial :: String -> (String, String) -> Readline m (Maybe String)
  GetInputChar :: String -> Readline m (Maybe Char)
  GetPassword :: Maybe Char -> String -> Readline m (Maybe String)
  WaitForAnyKey :: String -> Readline m Bool
  OutputStr :: String -> Readline m ()

-- TODO(Devin): add these two values as well
-- WithInterrupt :: m a -> Readline m a
-- HandleInterrupt :: m a -> m a -> Readline m a

makeSem ''Readline

outputStrLn :: Member Readline r => String -> Sem r ()
outputStrLn str = outputStr (str <> "\n")

-- | The simplest way to run a Readline effect. Immediately eliminates the
-- resulting 'H.InputT'. There is one problem with this approach however.
-- Internal details of polysemy cause 'H.runInputT' to be run once per effect
-- call (e.g. @getInputLine "> " >> getInputLine "> "@ will result in two calls
-- to 'H.runInputT'), and the History state of consecutive runs is not
-- preserved unless there is a history file. If you want history for your repl
-- there are therefore two recommended approaches:
--
-- * Provide a history file in the settings you specify. e.g.
-- @runReadline ('H.defaultSettings' {historyFile = Just ".repl_history"})@.
-- This is the easiest approach but technically suboptimal because the history
-- file will be read between every different primitive effect call.
-- * Use 'interpretReadlineAsInputT' or 'runReadlineFinal' and keep the
-- `H.InputT` around until after using 'runFinal' to escape polysemy land. This
-- way state can be preserved between effect calls. For an example using this
-- see @examples/Echo.hs@.
runReadline ::
  forall m r a.
  (MonadIO m, MonadMask m, Member (Embed m) r) =>
  H.Settings m ->
  Sem (Readline : r) a ->
  Sem r a
runReadline settings =
  runEmbedded (H.runInputT settings)
    . interpretReadlineAsInputT
    . raiseUnder @(Embed (H.InputT m))

-- | Interpreter for the somewhat common case of wanting to keep InputT around
-- until after 'runFinal' to ensure that state is preserved between subsequent
-- effects.
runReadlineFinal ::
  forall m r a.
  (MonadIO m, MonadMask m, Member (Final (H.InputT m)) r) =>
  Sem (Readline : r) a ->
  Sem r a
runReadlineFinal =
  embedToFinal
    . interpretReadlineAsInputT
    . raiseUnder @(Embed (H.InputT m))

-- | Interpret in terms of an embedded 'H.InputT' stack.
interpretReadlineAsInputT ::
  forall m r a.
  (MonadIO m, MonadMask m, Member (Embed (H.InputT m)) r) =>
  Sem (Readline : r) a ->
  Sem r a
interpretReadlineAsInputT = interpret $ \case
  GetInputLine prompt -> embed $ H.getInputLine prompt
  GetInputLineWithInitial prompt initial ->
    embed $ H.getInputLineWithInitial prompt initial
  GetInputChar prompt -> embed $ H.getInputChar prompt
  GetPassword maskChar prompt -> embed $ H.getPassword maskChar prompt
  WaitForAnyKey prompt -> embed $ H.waitForAnyKey prompt
  OutputStr str -> embed $ H.outputStr str
