import XMonad
import XMonad.ManageHook

import qualified XMonad.StackSet as W

import XMonad.Util.EZConfig(additionalKeys, additionalKeysP)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.NamedScratchpad

import XMonad.Config.Desktop

import XMonad.Layout.SimpleDecoration
import XMonad.Layout.Tabbed
import XMonad.Layout.Grid

import XMonad.Actions.WindowBringer
import XMonad.Actions.Navigation2D

import XMonad.Hooks.XPropManage
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops        (ewmh)

import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.Ssh

import Data.List
import Data.Default

import Graphics.X11.ExtraTypes.XF86

import System.IO

import System.Taffybar.Support.PagerHints (pagerHints)

main =
  xmonad $ docks $ ewmh $ pagerHints $
    def 
    { modMask = myModMask
    , terminal = "~/.resources/scripts/termite-wrapper.sh"
    , borderWidth = 1
    , focusedBorderColor = "#FFFFFF"
    , normalBorderColor = "#222222"
    , focusFollowsMouse = False
    , layoutHook = myLayoutHook 
    , handleEventHook    = handleEventHook def <+> docksEventHook
    , manageHook = manageHook def <+> manageDocks <+> namedScratchpadManageHook scratchpads <+> myManageHook
    , startupHook = do 
                setWMName "LG3D"
                docksStartupHook
                myStartup
    } `additionalKeysP` myKeys

myStartup = spawn "systemctl --user start wm.target"

myModMask = mod4Mask -- Use Super instead of Alt

myLayoutHook = avoidStruts $ layoutHook def ||| simpleTabbed ||| GridRatio (4/3) 

myManageHook = composeOne
  [ manageIdeaCompletionWindow
  , isFullscreen -?> doFullFloat
  , isDialog     -?> doCenterFloat
  , transience
  ]

(~=?) :: Eq a => Query [a] -> [a] -> Query Bool
q ~=? x = fmap (substring x) q

manageIdeaCompletionWindow = (className ~=? "jetbrains-" <&&> isDialog) -?> doFloat


spotifySelector = className =? "Spotify"
spotifyCommand = "spotify"

substring :: Eq a => [a] -> [a] -> Bool
substring (x:xs) [] = False
substring xs ys
    | prefix xs ys = True
    | substring xs (tail ys) = True
    | otherwise = False

prefix :: Eq a =>  [a] -> [a] -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys

scratchpads = [ NS "htop" "termite -e htop" (title =? "htop") centeredFloat
              , NS "spotify" spotifyCommand spotifySelector centeredFloat
              ]

centeredFloat = customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)
              
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

    
          , ("M-M1-t", namedScratchpadAction scratchpads "htop")
          , ("M-M1-s", namedScratchpadAction scratchpads "spotify")

          -- , ("M-<Left>", screenGo L False)
          -- , ("M-<Right>", screenGo R False)
          
          , ("M-<Left>", windowGo L False)
          , ("M-<Right>", windowGo R False)
          , ("M-<Up>", windowGo U False)
          , ("M-<Down>", windowGo D False)
          
          , ("C-q", spawn  "~/.resources/scripts/noctrlq.sh")

          , ("M-<F1>", manPrompt def)
          , ("M-<F2>", sshPrompt def)
          ]
