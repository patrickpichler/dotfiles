{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Exception.Base
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Char8 as BS
import           Data.List
import           Data.List.Split
import qualified Data.Map as M
import           Data.Maybe
import qualified GitHub.Auth as Auth
import           StatusNotifier.Tray
import           System.Directory
import           System.Environment
import           System.FilePath.Posix
import           System.IO
import           System.Log.Handler.Simple
import           System.Log.Logger
import           System.Process
import           System.Taffybar
import           System.Taffybar.Auth
import           System.Taffybar.Context (appendHook)
import           System.Taffybar.DBus
import           System.Taffybar.DBus.Toggle
import           System.Taffybar.Hooks
import           System.Taffybar.Information.CPU
import           System.Taffybar.Information.EWMHDesktopInfo
import           System.Taffybar.Information.Memory
import           System.Taffybar.Information.X11DesktopInfo
import           System.Taffybar.SimpleConfig
import           System.Taffybar.Util
import           System.Taffybar.Widget
import           System.Taffybar.Widget.Generic.PollingGraph
import           System.Taffybar.Widget.Generic.PollingLabel
import           System.Taffybar.Widget.Util
import           System.Taffybar.Widget.Workspaces
import           Text.Printf
import Text.Read hiding (lift)

mkRGBA (r, g, b, a) = (r/256, g/256, b/256, a/256)
blue = mkRGBA (42, 99, 140, 256)
yellow1 = mkRGBA (242, 163, 54, 256)
yellow2 = mkRGBA (254, 204, 83, 256)
yellow3 = mkRGBA (227, 134, 18, 256)
red = mkRGBA (210, 77, 37, 256)

myGraphConfig =
  defaultGraphConfig
  { graphPadding = 0
  , graphBorderWidth = 0
  , graphWidth = 40
  , graphBackgroundColor = (1.0, 1.0, 1.0, 0.0)
  }

memCfg =
  myGraphConfig
  { graphDataColors = [(0.129, 0.588, 0.953, 1)]
  , graphLabel = Just "mem"
  }

memCallback :: IO [Double]
memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

notifySystemD = void $ runCommandFromPath ["systemd-notify", "--ready"]

main = do
  let cpuCfg =
        myGraphConfig
          { graphDataColors = [(0, 1, 0, 1), (1, 0, 1, 0.5)]
          , graphBackgroundColor = (1.0, 1.0, 1.0, 0.0)
          , graphLabel = Just "cpu"
          }
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      mem = pollingGraphNew memCfg 1 memCallback
      myWorkspacesConfig =
        defaultWorkspacesConfig
          { underlineHeight = 3
          , underlinePadding = 2
          , maxIcons = Just 0
          }
      workspaces = workspacesNew myWorkspacesConfig
      wnd = windowsNew defaultWindowsConfig
      baseConfig = defaultSimpleTaffyConfig
        { startWidgets =
          [ workspaces
          -- , mpris2New 
          ]
        , endWidgets = map (>>= buildContentsBox)
          [ textClockNew Nothing "%a %b %_d %r" 1
          , sniTrayNew 
          , textBatteryNew "$percentage$%"
          , batteryIconNew
          , cpu
          , mem
          , networkMonitorNew defaultNetFormat Nothing >>= setMinWidth 180
          , wnd
          ]
        , barPosition = Top
        , barPadding = 0
        , barHeight = 40


        , widgetSpacing = 0
        }

  startTaffybar $ 
    appendHook notifySystemD $
    appendHook (void $ getHost False) $
    withBatteryRefresh $ 
    withLogServer $ 
    withToggleServer $ 
    toTaffyConfig baseConfig
