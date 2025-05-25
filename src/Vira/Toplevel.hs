{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Vira.Toplevel (
  runVira,
) where

import Control.Exception (bracket)
import Effectful (Eff)
import Effectful.Reader.Dynamic (ask)
import Main.Utf8 qualified as Utf8
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Static (
  addBase,
  noDots,
  staticPolicy,
  (>->),
 )
import Paths_vira qualified
import Servant.Server.Generic (genericServe)
import Vira.App (AppStack, Settings (..))
import Vira.App qualified as App
import Vira.App.CLI (RepoSettings (..), Settings (..))
import Vira.App.CLI qualified as CLI
import Vira.Lib.BinaryCache (BinaryCacheConfig (..), loginAttic)
import Vira.Lib.BinaryCache qualified as BinaryCache
import Vira.App.LinkTo.Resolve (linkTo)
import Vira.App.Logging
import Vira.Routes qualified as Routes
import Vira.State.Core (closeViraState, openViraState)
import Vira.Supervisor qualified
import Prelude hiding (Reader, ask, runReader)

-- | Run the Vira application
runVira :: IO ()
runVira = do
  Utf8.withUtf8 $ do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    appIO =<< CLI.parseCLI
  where
    -- Like `app` but in `IO`
    appIO :: Settings -> IO ()
    appIO settings = do
      let repos = settings.repo.cloneUrls
      bracket (openViraState repos) closeViraState $ \acid -> do
        supervisor <- Vira.Supervisor.newSupervisor

        -- Determine BinaryCacheConfig from CLI settings
        let cliCacheProvider = settings.repo.cliBinaryCacheProvider
        let effectiveBinaryCacheConfig = case cliCacheProvider of
              Just (CLI.CLIUseCachix cachixCliCfg) ->
                UseCachix $ BinaryCache.CachixConfig {BinaryCache.cachixCacheName = cachixCliCfg.cachixName}
              Just (CLI.CLIUseAttic atticCliCfg) ->
                UseAttic $ BinaryCache.AtticConfig
                  { BinaryCache.atticLoginName = atticCliCfg.atticCliLoginName
                  , BinaryCache.atticCacheName = atticCliCfg.atticCliCacheName
                  , BinaryCache.atticCacheUrl = atticCliCfg.atticCliCacheUrl
                  , BinaryCache.atticTokenEnvVar = atticCliCfg.atticCliTokenEnvVar
                  }
              Nothing -> NoBinaryCache

        let st = App.AppState {linkTo = linkTo, settings = settings, acid = acid, supervisor = supervisor, effectiveBinaryCacheConfig = effectiveBinaryCacheConfig}

        -- Perform Attic login if configured
        App.runApp st $ do
          case effectiveBinaryCacheConfig of
            UseAttic atticCfg -> do
              logInfo $ "Attic cache configured. Attempting login for " <> atticCfg.atticLoginName
              loginAttic atticCfg -- This function is already in Eff AppStack
            _ -> pure ()
          app settings

    -- Vira application for given `Settings`
    app :: (HasCallStack) => Settings -> Eff AppStack ()
    app settings = do
      log Info $ "Launching vira (" <> settings.instanceName <> ") at http://" <> settings.host <> ":" <> show settings.port
      log Debug $ "Settings: " <> show settings
      staticDir <- liftIO Paths_vira.getDataDir
      log Debug $ "Serving static files from: " <> show staticDir
      let staticMiddleware = staticPolicy $ noDots >-> addBase staticDir
      cfg <- ask
      let servantApp = genericServe $ Routes.handlers cfg
      let host = fromString $ toString settings.host
      let warpSettings = Warp.defaultSettings & Warp.setHost host & Warp.setPort settings.port
      liftIO $ Warp.runSettings warpSettings $ staticMiddleware servantApp
