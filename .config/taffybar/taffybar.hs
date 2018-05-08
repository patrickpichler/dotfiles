{-# LANGUAGE PackageImports #-}
module Main where

import qualified        Control.Concurrent.MVar as MV
import                  Control.Exception.Base
import                  Control.Monad
import                  Control.Monad.Reader
import                  Control.Monad.Trans
import qualified        Data.ByteString.Char8 as BS
import                  Data.GI.Base
import                  Data.GI.Base.ManagedPtr
import                  Data.List
import                  Data.List.Split
import qualified        Data.Map as M
import                  Data.Maybe
import                  Debug.Trace
import                  Foreign.ForeignPtr
import                  Foreign.Ptr
import qualified        GI.Gtk as GI
import qualified "gtk3" Graphics.UI.Gtk as Gtk
import qualified "gtk3" Graphics.UI.Gtk.Abstract.Widget as W
import qualified "gtk3" Graphics.UI.Gtk.Layout.Table as T
import                  Graphics.UI.Gtk.Types
import                  StatusNotifier.Tray
import                  System.Directory
import                  System.Environment
import                  System.FilePath.Posix
import                  System.Glib.GObject
import                  System.IO
import                  System.Log.Handler.Simple
import                  System.Log.Logger
import                  System.Process
import                  System.Taffybar
import                  System.Taffybar.Auth
import                  System.Taffybar.Compat.GtkLibs
import                  System.Taffybar.DBus
import                  System.Taffybar.DBus.Toggle
import                  System.Taffybar.Hooks
import                  System.Taffybar.IconImages
import                  System.Taffybar.Information.CPU
import                  System.Taffybar.Information.EWMHDesktopInfo
import                  System.Taffybar.Information.Memory
import                  System.Taffybar.Information.X11DesktopInfo
import                  System.Taffybar.SimpleConfig
import                  System.Taffybar.Widget
import                  System.Taffybar.Widget.Generic.PollingGraph
import                  System.Taffybar.Widget.Workspaces
import                  Text.Printf
import                  Unsafe.Coerce

buildPadBoxNoShrink orig  = liftIO $  buildPadBox orig

setMinWidth width widget = liftIO $ do
  Gtk.widgetSetSizeRequest widget width (-1)
  return widget

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

containerAddReturn c w =
  Gtk.containerAdd c w >> Gtk.widgetShowAll c >> return (Gtk.toWidget c)

underlineWidget cfg buildWidget name = do
  w <- buildWidget
  t <- T.tableNew 2 1 False
  u <- Gtk.eventBoxNew

  W.widgetSetSizeRequest u (-1) $ underlineHeight cfg

  T.tableAttach t w 0 1 0 1 [T.Expand] [T.Expand] 0 0
  T.tableAttach t u 0 1 1 2 [T.Fill] [T.Shrink] 0 0

  Gtk.widgetSetName u (printf "%s-underline" name :: String)

  Gtk.widgetShowAll t

  return $ Gtk.toWidget t


logDebug = do
  handler <- streamHandler stdout DEBUG
  logger <- getLogger "System.Taffybar"
  saveGlobalLogger $ setLevel DEBUG logger
  infoLogger <- getLogger "System.Information"
  saveGlobalLogger $ setLevel DEBUG infoLogger

main = do
  let cpuCfg =
        myGraphConfig
        { graphDataColors = [(0, 1, 0, 1), (1, 0, 1, 0.5)]
        , graphBackgroundColor = (1.0, 1.0, 1.0, 0.0)
        , graphLabel = Just "cpu"
        }
      clock = textClockNew Nothing "%a %b %_d %r" 1
      mpris = mpris2New
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      mem = pollingGraphNew memCfg 1 memCallback
      battery = batteryBarNew defaultBatteryConfig 1.0
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
            , mpris >>= buildPadBox
            ]
        , endWidgets = map (>>= buildPadBoxNoShrink)
          [ clock >>= setMinWidth 200
          , sniTrayNew
          , battery
          , cpu
          , mem
          , networkMonitorNew defaultNetFormat Nothing >>= setMinWidth 180
          , wnd
          ]
        , barPosition = Top
        , barPadding = 0
        , barHeight = underlineHeight myWorkspacesConfig + windowIconSize myWorkspacesConfig + 15
        , widgetSpacing = 0
        }
  dyreTaffybar $ withLogServer $ withToggleServer $ toTaffyConfig baseConfig
