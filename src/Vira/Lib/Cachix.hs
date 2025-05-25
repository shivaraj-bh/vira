{-# LANGUAGE TemplateHaskell #-}

-- | Working with cachix
module Vira.Lib.Cachix where

import System.Process (CreateProcess, proc)
import System.Which (staticWhich)

{- | Path to the `cachix` executable

This should be available in the PATH, thanks to Nix and `which` library.
-}
cachixBin :: FilePath
cachixBin = $(staticWhich "cachix")

cachixPushProcess :: Text -> FilePath -> CreateProcess
cachixPushProcess cache path = proc cachixBin ["push", "-v", toString cache, path]

{- | Path to the `attic` executable

This should be available in the PATH, thanks to Nix and `which` library.
-}
atticBin :: FilePath
atticBin = $(staticWhich "attic")

atticLoginProcess :: Text -> Text -> Text -> CreateProcess
atticLoginProcess loginName cacheUrl token =
  proc atticBin ["login", toString loginName, toString cacheUrl, toString token]

atticPushProcess :: Text -> FilePath -> CreateProcess
atticPushProcess cache path = proc atticBin ["push", toString cache, path]
