import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab

import XMonad.Util.EZConfig (additionalKeysP)

import XMonad.Layout.ThreeColumns

import XMonad.Hooks.EwmhDesktops

import XMonad.Hooks.DynamicLog

import XMonad.Util.SpawnOnce

myTerminal = "alacritty"

myLayout = tiled ||| Mirror tiled ||| Full
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1      -- Default number of windows in the master pane
    ratio   = 1/2    -- Default proportion of screen occupied by master pane
    delta   = 3/100  -- Percent of screen to increment by when resizing panes

myConfig = def
    { modMask    = mod4Mask  -- Rebind Mod to the Super key
    , layoutHook = myLayout  -- Use custom layouts
    , handleEventHook = handleEventHook def <+> fullscreenEventHook
    , startupHook = myStartupHook
    }
  `additionalKeysP`
    [ ("M-S-z", spawn "xscreensaver-command -lock")
    , ("M-C-s", unGrab *> spawn "scrot -s"        )
    , ("M-f"  , spawn "firefox"                   )
    ]

myStartupHook :: X ()
myStartupHook = do
    spawnOnce "xrandr --output \"eDP-1\" --mode \"1920x1200\" --rate \"60\" --scale \"1x1\""

main :: IO ()
main = xmonad . ewmh =<< xmobar myConfig
