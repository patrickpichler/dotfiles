import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig(additionalKeys, additionalKeysP)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Config.Desktop
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import System.IO
import Graphics.X11.ExtraTypes.XF86
import Data.Default
import XMonad.Hooks.EwmhDesktops        (ewmh)
import XMonad.Hooks.ManageDocks
import System.Taffybar.Hooks.PagerHints (pagerHints)
import XMonad.Actions.WindowBringer
import XMonad.Layout.SimpleDecoration

main = do
  xmproc <- spawnPipe "/home/patrick/.asdf/shims/my-taffybar"

  xmonad $ docks $ ewmh $ pagerHints $ desktopConfig
    { modMask = myModMask
    , terminal = "termite"
    , borderWidth = 1 
    , focusedBorderColor = "#FFFFFF"
    , normalBorderColor = "#222222"
    , focusFollowsMouse = False
    , layoutHook = myLayoutHook 
    , handleEventHook    = handleEventHook def <+> docksEventHook
    , manageHook = manageHook def <+> manageDocks <+> myManageHook
    , startupHook = do 
                setWMName "LG3D"
                docksStartupHook
    } `additionalKeysP` myKeys

myModMask = mod4Mask -- Use Super instead of Alt

myLayoutHook = avoidStruts $ layoutHook def

myManageHook = composeAll
  [ isDialog     --> doCenterFloat
  , isFullscreen --> doFullFloat
  ]

myKeys =  [ ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +1.5%")
          , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@  -1.5%")
          , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")    

          , ("<XF86AudioPlay>", spawn "playerctl play-pause")    
          , ("<XF86AudioPrev>", spawn "playerctl previous")    
          , ("<XF86AudioNext>", spawn "playerctl next")    
          
          , ("<XF86MonBrightnessUp>", spawn "lux -a 5%")    
          , ("<XF86MonBrightnessDown>", spawn "lux -s 5%")    

          , ("M-S-l", spawn "i3lock -c 000000")
          , ("M-s", bringMenu)
          , ("M-S-s", gotoMenu)
          ]
