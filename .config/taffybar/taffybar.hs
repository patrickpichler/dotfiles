module Main where

import System.Taffybar
import System.Taffybar.SimpleConfig
import System.Taffybar.Widget.SNITray
import System.Taffybar.Widget.Workspaces
import System.Taffybar.Widget.SimpleClock
import System.Taffybar.Widget.Generic.PollingGraph
import System.Taffybar.Information.CPU

cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [ totalLoad, systemLoad ]

main = do
  let cpuCfg = defaultGraphConfig { graphDataColors = [ (0, 1, 0, 1), (1, 0, 1, 0.5)]
                                  , graphLabel = Just "cpu"
                                  }
      clock = textClockNew Nothing "<span fgcolor='orange'>%a %b %_d %H:%M</span>" 1
      tray = sniTrayNew
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      workspaces = workspacesNew defaultWorkspacesConfig
      simpleConfig = defaultSimpleTaffyConfig
                       { startWidgets = [ workspaces ]
                       , endWidgets = [ tray, clock, cpu ]
                       }
  simpleTaffybar simpleConfig
