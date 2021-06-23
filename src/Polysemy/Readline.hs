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
  ( -- * effect + actions
    Readline (..),
    getInputLine,
    getInputLineWithInitial,
    getInputChar,
    getPassword,
    waitForAnyKey,
    outputStr,
    outputStrLn,

    -- * interpreters
    runReadline,
    interpretReadlineAsInputT,

    -- * re-exports
    H.Settings,
    H.defaultSettings,
  )
where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Polysemy
import Polysemy.Embed
import qualified System.Console.Haskeline as H

-- | See documentation in haskeline package for the corresponding functions with
-- the same name.
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

-- | The standard way to run a Readline effect. Should be sufficient for
-- most use cases. If you want to modify the Behavior or Prefs of InputT use
-- interpretReadlineAsInputT instead.
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
