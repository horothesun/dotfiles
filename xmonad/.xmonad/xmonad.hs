import XMonad

import Graphics.X11.ExtraTypes.XF86
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.ThreeColumns
import XMonad.Util.EZConfig
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce
import XMonad.Util.Ungrab

myTerminal = "alacritty"
audioCardId = "1"
internalMonitorBacklightDeviceName = "intel_backlight"
internalKeyboardBacklightDeviceName = "smc::kbd_backlight"
internalMonitorSetResolutionCommand = "xrandr --output eDP1 --mode 1920x1200 --rate 60 --scale 1x1"
setKeyboardRepeatDelayAndRateCommand = "xset r rate 280 60"

myLayout = tiled ||| Mirror tiled ||| Full
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1     -- Default number of windows in the master pane
    ratio   = 1/2   -- Default proportion of screen occupied by master pane
    delta   = 5/100 -- Percent of screen to increment by when resizing panes

myStartupHook :: X ()
myStartupHook = do
  spawnOnce internalMonitorSetResolutionCommand
  spawnOnce "volumeicon &"
  spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 0 --tint 0x282c34 --height 22 &"
  spawnOnce setKeyboardRepeatDelayAndRateCommand

myManageHook :: ManageHook
myManageHook = composeAll
  [ className =? "Gimp" --> doFloat
  , isDialog            --> doFloat
  ]

myKeys :: [(String, X ())]
myKeys =
  [ ("M-w", spawn "brave &")
  , ("<XF86MonBrightnessUp>",   spawn $ "brightnessctl --quiet --device " ++ internalMonitorBacklightDeviceName ++ " set 5%+")
  , ("<XF86MonBrightnessDown>", spawn $ "brightnessctl --quiet --device " ++ internalMonitorBacklightDeviceName ++ " set 5%-")
  , ("<XF86AudioMute>",         spawn $ "toggle_audio.sh " ++ audioCardId)
  , ("<XF86AudioRaiseVolume>",  spawn $ "amixer --card " ++ audioCardId ++ " sset Master 5%+")
  , ("<XF86AudioLowerVolume>",  spawn $ "amixer --card " ++ audioCardId ++ " sset Master 5%-")
  , ("<XF86KbdBrightnessUp>",   spawn $ "sudo brightnessctl --quiet --device " ++ internalKeyboardBacklightDeviceName ++ " set 5%+")
  , ("<XF86KbdBrightnessDown>", spawn $ "sudo brightnessctl --quiet --device " ++ internalKeyboardBacklightDeviceName ++ " set 5%-")
  ]

myXmobarPP :: PP
myXmobarPP = def
  { ppSep             = magenta " • "
  , ppTitleSanitize   = xmobarStrip
  , ppCurrent         = wrap (blue "[") (blue "]")
  , ppHidden          = white . wrap " " ""
  , ppHiddenNoWindows = lowWhite . wrap " " ""
  , ppUrgent          = red . wrap (yellow "!") (yellow "!")
  } where
    -- Windows should have *some* title, which should not not exceed a sane length
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 24
    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

myConfig = def
  { modMask     = mod4Mask -- Rebind Mod to the Super key
  , terminal    = myTerminal
  , layoutHook  = myLayout
  , startupHook = myStartupHook
  , manageHook  = myManageHook -- Match on certain windows
  , handleEventHook = handleEventHook def <+> fullscreenEventHook
  } `additionalKeysP` myKeys

main :: IO ()
main = xmonad . ewmh =<< statusBar "xmobar" myXmobarPP toggleStrutsKey myConfig
  where
    toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
    toggleStrutsKey XConfig{ modMask = m } = (m, xK_b)
