import XMonad
import XMonad.Config.Xfce
import XMonad.Util.EZConfig (additionalKeys)
import qualified Data.Map as M

main = xmonad $ xfceConfig
  { modMask = mod4Mask
  , terminal = "xfce4-terminal"}
  `additionalKeys` myKeys

myKeys = [((mod4Mask, xK_p), spawn "xfce4-popup-whiskermenu")]
