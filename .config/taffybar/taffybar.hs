module Main where

import System.Information.CPU
import System.Information.Memory
import System.Information.Battery
import System.Taffybar
import System.Taffybar.Pager
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.Battery
import System.Taffybar.MPRIS2
import System.Taffybar.SimpleClock
import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.Widgets.PollingBar
import System.Taffybar.Widgets.PollingGraph
import System.Taffybar.WorkspaceHUD
import System.Taffybar.WorkspaceSwitcher
import System.Taffybar.WindowSwitcher

import qualified Graphics.UI.Gtk as Gtk

memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback = do
  (userLoad, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

main = do
  pager <- pagerNew defaultPagerConfig { activeWindow = escape . shorten 20 }


  let memCfg = defaultGraphConfig { graphDataColors = [(1, 0, 0, 1)]
                                  , graphLabel = Just "mem"
                                  }
      cpuCfg = defaultGraphConfig { graphDataColors = [ (0, 1, 0, 1)
                                                      , (1, 0, 1, 0.5)
                                                      ]
                                  , graphLabel = Just "cpu"
                                  }
      batCfg = defaultBatteryConfig { barPadding = 1
                                    , barColor   = batColor
                                    , barBorderColor = (1,1,1)
                                    }
  let clock = textClockNew Nothing "<span fgcolor='orange'>%a %b %_d %H:%M</span>" 1
      wss = wspaceSwitcherNew pager
      wnd = windowSwitcherNew pager
      note = notifyAreaNew defaultNotificationConfig
      mpris = mpris2New 
      mem = pollingGraphNew memCfg 1 memCallback
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      tray = systrayNew
      bat = batteryBarNew batCfg 5
  defaultTaffybar defaultTaffybarConfig { startWidgets = [ wss, mpris ]
                                        , endWidgets = [ tray,  clock, mem, cpu, bat, wnd, note ]
                                        }


batColor :: Double -> (Double, Double, Double)
batColor p = if p < 1 then (r, g, 0) else (0.7, 0.7, 0.7)
  where r = if p < 0.5 then 1.0 else 1.0 - (p * 100 - 50) * 5.12/256.0
        g = if p > 0.5 then 1.0 else p * 100 * 5.12/256.0

