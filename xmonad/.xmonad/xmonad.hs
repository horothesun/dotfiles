import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.ThreeColumns
import XMonad.Util.EZConfig
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.SpawnOnce
import XMonad.Util.Ungrab

myLayout = tiled ||| Mirror tiled ||| Full
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1     -- Default number of windows in the master pane
    ratio   = 1/2   -- Default proportion of screen occupied by master pane
    delta   = 5/100 -- Percent of screen to increment by when resizing panes

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "xrandr --output eDP-1 --mode 1920x1200 --rate 60 --scale 1x1"

myManageHook :: ManageHook
myManageHook = composeAll
  [ className =? "Gimp" --> doFloat
  , isDialog            --> doFloat
  ]

myKeys :: [(String, X ())]
myKeys =
  [ ("M-w", spawn "brave &" )
  ]

myConfig = def
  { modMask     = mod4Mask      -- Rebind Mod to the Super key
  , terminal    = "alacritty"
  , layoutHook  = myLayout      -- Use custom layouts
  , startupHook = myStartupHook
  , manageHook  = myManageHook  -- Match on certain windows
  , handleEventHook = handleEventHook def <+> fullscreenEventHook
  } `additionalKeysP` myKeys

main :: IO ()
-- main = xmonad . ewmh =<< xmobar myConfig

main = xmonad
     . ewmh
   =<< statusBar "xmobar" def toggleStrutsKey myConfig
  where
    toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
    toggleStrutsKey XConfig{ modMask = m } = (m, xK_b)
