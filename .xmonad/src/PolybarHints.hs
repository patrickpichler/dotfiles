module PolybarHints (
  eventLogHook
) where

import Data.List (sortBy)
import Data.Function (on)
import Control.Monad (forM_, join)
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.NamedWindows (getName)
import qualified XMonad.StackSet as S
import XMonad

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

eventLogHook :: D.Client -> X ()
eventLogHook dbus = do
  winset <- gets windowset

  let visibleWorkspaces = visibles winset
  let currWs = S.currentTag winset
  let wss = map S.tag $ S.workspaces winset
  let wsStr = join $ map (fmt currWs visibleWorkspaces) $ sort' wss

  io (dbusOutput dbus wsStr)

  where fmt currWs visibleWs ws
          | currWs == ws = " [" ++ ws ++ "] "
          | ws `elem` visibleWs = " <" ++ ws ++ "> "
          | otherwise    = " " ++ ws ++ " "
        sort' = sortBy (compare `on` (!! 0))
        visibles s = map (S.tag . S.workspace) (S.visible s)
