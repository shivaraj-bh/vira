{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-} -- Added for Text literals
{-# LANGUAGE DeriveDataTypeable #-} -- Added for Exception Typeable
{-# LANGUAGE DeriveGeneric #-} -- Added for Generic derivation

-- | Working with binary caches like Cachix or Attic
module Vira.Lib.BinaryCache where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.:), (.:?))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable) -- Added for Exception Typeable
import Effectful (Eff, IOE, (:>))
-- Assuming you have a variant that doesn't need stdin and ignores output,
-- or adapt as needed. If procStrictWithNonNullExitCode_ is not available,
-- you might need to use procStrictWithNonNullExitCode and ignore its result.
import Effectful.Process (Process, procStrictWithNonNullExitCode)
import GHC.Generics (Generic)
import System.Environment (lookupEnv)
import UnliftIO.Exception (Exception, throwString) -- Or your preferred error handling
import Vira.App.Logging (Log, Message, logInfo, logError) -- Assuming these are your logging functions
-- toString is available from Relude (Prelude)

data AtticConfig = AtticConfig
  { atticLoginName :: Text
  , atticCacheName :: Text
  , atticCacheUrl  :: Text
  , atticTokenEnvVar :: Maybe Text -- Optional: custom env var name for the token
  } deriving (Show, Eq, Generic)

instance FromJSON AtticConfig where
  parseJSON = withObject "AtticConfig" $ \o -> AtticConfig
    <$> o .: "loginName"
    <*> o .: "cacheName"
    <*> o .: "cacheUrl"
    <*> o .:? "tokenEnvVar"
instance ToJSON AtticConfig

data CachixConfig = CachixConfig
  { cachixCacheName :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON CachixConfig where
  parseJSON = withObject "CachixConfig" $ \o -> CachixConfig
    <$> o .: "cacheName"
instance ToJSON CachixConfig

data BinaryCacheConfig
  = UseCachix CachixConfig
  | UseAttic AtticConfig
  | NoBinaryCache -- Option to disable binary caching
  deriving (Show, Eq, Generic)

-- Example FromJSON:
-- { "cachix": { "cacheName": "mycache" } }
-- { "attic": { "loginName": "user", "cacheName": "mycache", "cacheUrl": "http://..." } }
-- { "none": {} } or just omitting the binaryCache field in top-level config could default to NoBinaryCache
instance FromJSON BinaryCacheConfig where
  parseJSON = withObject "BinaryCacheConfig" $ \o ->
    (UseCachix <$> o .: "cachix")
    <|> (UseAttic <$> o .: "attic")
    <|> (do n <- o .: "none"; if n then pure NoBinaryCache else fail "expected true for none")
    <|> pure NoBinaryCache -- Default to NoBinaryCache if no known key is present
instance ToJSON BinaryCacheConfig

newtype BinaryCacheException = BinaryCacheException String
  deriving (Show, Typeable)
instance Exception BinaryCacheException

-- | Logs into Attic using the provided configuration.
-- Reads the token from ATTIC_LOGIN_TOKEN or a custom env var.
loginAttic :: (IOE :> es, Process :> es, Log Message :> es) => AtticConfig -> Eff es ()
loginAttic AtticConfig{..} = do
  let tokenEnvName = T.unpack $ fromMaybe "ATTIC_LOGIN_TOKEN" atticTokenEnvVar
  logInfo $ "Attempting Attic login for user " <> atticLoginName <> " to cache " <> atticCacheName <> " at " <> atticCacheUrl <> " using token from " <> T.pack tokenEnvName
  mLoginToken <- liftIO $ lookupEnv tokenEnvName
  case mLoginToken of
    Nothing -> do
      let errMsg = "Attic token environment variable (" <> T.pack tokenEnvName <> ") not set. Cannot login to Attic."
      logError errMsg
      throwString $ T.unpack errMsg
    Just token -> do
      logInfo "Attic token found. Proceeding with attic login."
      void $ procStrictWithNonNullExitCode "attic" [toString atticLoginName, toString atticCacheUrl, token]
      logInfo "Attic login command executed."

-- | Pushes store paths to the configured binary cache.
push :: (IOE :> es, Process :> es, Log Message :> es) => BinaryCacheConfig -> [FilePath] -> Eff es ()
push NoBinaryCache _storePaths = logInfo "Binary caching is disabled. Skipping push."
push (UseCachix CachixConfig{..}) storePaths = do
  logInfo $ "Pushing to Cachix cache: " <> cachixCacheName
  -- CACHIX_AUTH_TOKEN is expected to be in the environment.
  void $ procStrictWithNonNullExitCode "cachix" (["push", toString cachixCacheName] ++ storePaths)
  logInfo "Successfully pushed to Cachix."
push (UseAttic AtticConfig{..}) storePaths = do
  let fullAtticCacheName = atticLoginName <> ":" <> atticCacheName
  logInfo $ "Pushing to Attic cache: " <> fullAtticCacheName
  -- Attic login should have been performed separately if needed.
  void $ procStrictWithNonNullExitCode "attic" (["push", toString fullAtticCacheName] ++ storePaths)
  logInfo "Successfully pushed to Attic."
