--  Make sure xfdesktop is disabled from autostart, or uninstalled,
--  since it may prevent xfce-panel from appearing once xmonad is started.

import XMonad
import XMonad.Config.Xfce
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Actions.SpawnOn (spawnOn)
import qualified Data.Map as M

-- helpful: https://gist.github.com/jsjolund/94f6821b248ff79586ba

main = xmonad $ xfceConfig
  { modMask = mod4Mask
  , terminal = "xfce4-terminal"
  , workspaces = map show [1 .. 9 :: Int]
  , startupHook = do
      spawnOn "1" "firefox"
      spawnOn "2" "xfce4-terminal"
      spawnOn "2" "emacs"
      spawnOn "3" "thunar"
      spawnOn "3" "keepassx" }
  `additionalKeys` myKeys

myKeys = [((mod4Mask, xK_p), spawn "exe=$(dmenu_path | dmenu) && eval \"exec $exe\"")]
