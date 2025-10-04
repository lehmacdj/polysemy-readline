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
    withInterrupt,
    handleInterrupt,

    -- * Interpreters
    runReadline,
    runReadlineFinal,

    -- * Re-exports from @haskeline@
    H.Settings (..),
    H.defaultSettings,
    H.runInputT,

    -- * Deprecated
    interpretReadlineAsInputT,
  )
where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Polysemy
import Polysemy.Final
import qualified System.Console.Haskeline as H

-- | For documentation on actions see haskeline's functions with the same name
-- and similar type signatures.
data Readline m a where
  GetInputLine :: String -> Readline m (Maybe String)
  GetInputLineWithInitial :: String -> (String, String) -> Readline m (Maybe String)
  GetInputChar :: String -> Readline m (Maybe Char)
  GetPassword :: Maybe Char -> String -> Readline m (Maybe String)
  WaitForAnyKey :: String -> Readline m Bool
  OutputStr :: String -> Readline m ()
  WithInterrupt :: m a -> Readline m a
  HandleInterrupt :: m a -> m a -> Readline m a

makeSem ''Readline

outputStrLn :: (Member Readline r) => String -> Sem r ()
outputStrLn str = outputStr (str <> "\n")

-- | The simplest way to run a Readline effect. Runs the entire Readline
-- computation in a single 'H.runInputT' call, preserving history state between
-- effect calls. This is the recommended approach for most use cases.
--
-- For more control over when 'H.runInputT' is invoked, use 'runReadlineFinal'
-- directly and manage the 'H.InputT' context yourself. See @examples/Echo.hs@
-- for an example.
runReadline ::
  forall m a.
  (MonadIO m, MonadMask m) =>
  H.Settings m ->
  Sem '[Readline] a ->
  m a
runReadline settings sem =
  H.runInputT settings $
    runFinal $
      runReadlineFinal $
        raiseUnder @(Final (H.InputT m)) sem

-- | Interpreter for the somewhat common case of wanting to keep InputT around
-- until after 'runFinal' to ensure that state is preserved between subsequent
-- effects.
runReadlineFinal ::
  forall m r a.
  (MonadIO m, MonadMask m, Member (Final (H.InputT m)) r) =>
  Sem (Readline : r) a ->
  Sem r a
runReadlineFinal = interpretFinal $ \case
  GetInputLine prompt -> liftS $ H.getInputLine prompt
  GetInputLineWithInitial prompt initial -> liftS $ H.getInputLineWithInitial prompt initial
  GetInputChar prompt -> liftS $ H.getInputChar prompt
  GetPassword maskChar prompt -> liftS $ H.getPassword maskChar prompt
  WaitForAnyKey prompt -> liftS $ H.waitForAnyKey prompt
  OutputStr str -> liftS $ H.outputStr str
  WithInterrupt action -> do
    action' <- runS action
    pure $ H.withInterrupt action'
  HandleInterrupt handler action -> do
    handler' <- runS handler
    action' <- runS action
    pure $ H.handleInterrupt handler' action'

-- | Interpret in terms of an embedded 'H.InputT' stack.
interpretReadlineAsInputT ::
  forall m r a.
  (MonadIO m, MonadMask m, Member (Final (H.InputT m)) r) =>
  Sem (Readline : r) a ->
  Sem r a
interpretReadlineAsInputT = runReadlineFinal
{-# DEPRECATED interpretReadlineAsInputT "Use runReadlineFinal instead" #-}
