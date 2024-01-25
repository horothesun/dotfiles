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
setKeyboardRepeatDelayAndRateCommand = "xset r rate 280 40"
disableTouchpadTapToClick = "synclient MaxTapTime=0 &"
launchNetworkApplet = "nm-applet &"
launchVolumeApplet = "volumeicon &"
launchClipboardManager = "copyq &"
launchSysTray = "trayer --edge top --align right --widthtype request --padding 5 " ++
  "--SetDockType true --SetPartialStrut true --expand true --monitor 1 " ++
  "--transparent true --alpha 0 --tint 0x282c34 --height 26 --iconspacing 5 &"

bashScreenshotName = "\"${HOME}/Downloads/Screenshot $(date -u \"+%Y-%m-%d at %H.%M.%S\").png\""

myLayout = tiled ||| Mirror tiled ||| Full
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1     -- Default number of windows in the master pane
    ratio   = 1/2   -- Default proportion of screen occupied by master pane
    delta   = 5/100 -- Percent of screen to increment by when resizing panes

myStartupHook :: X ()
myStartupHook = do
  spawnOnce internalMonitorSetResolutionCommand
  spawnOnce setKeyboardRepeatDelayAndRateCommand
  spawnOnce disableTouchpadTapToClick
  spawnOnce launchNetworkApplet
  spawnOnce launchVolumeApplet
  spawnOnce launchClipboardManager
  spawnOnce launchSysTray
  -- spawnOnce "sxhkd &"

myManageHook :: ManageHook
myManageHook = composeAll
  [ className =? "Gimp" --> doFloat
  , isDialog            --> doFloat
  ]

myKeys :: [(String, X ())]
myKeys =
  [ ("<XF86MonBrightnessUp>",   spawn $ "brightnessctl --quiet --device " ++ internalMonitorBacklightDeviceName ++ " set 2%+")
  , ("<XF86MonBrightnessDown>", spawn $ "brightnessctl --quiet --device " ++ internalMonitorBacklightDeviceName ++ " set 2%-")
  , ("<XF86KbdBrightnessUp>",   spawn $ "sudo brightnessctl --quiet --device " ++ internalKeyboardBacklightDeviceName ++ " set 2%+")
  , ("<XF86KbdBrightnessDown>", spawn $ "sudo brightnessctl --quiet --device " ++ internalKeyboardBacklightDeviceName ++ " set 2%-")
  , ("<XF86AudioMute>",         spawn $ "toggle_audio.sh " ++ audioCardId)
  , ("<XF86AudioRaiseVolume>",  spawn $ "amixer --card " ++ audioCardId ++ " sset Master 5%+")
  , ("<XF86AudioLowerVolume>",  spawn $ "amixer --card " ++ audioCardId ++ " sset Master 5%-")
  -- launch browser
  , ("M-w",     spawn "brave &")
  -- screenshot from current window to file
  , ("M-C-2",   spawn $ "maim --window $(xdotool getactivewindow) " ++ bashScreenshotName ++ " &")
  -- screenshot from current window to clipboard
  , ("M-C-S-2", spawn "maim --window $(xdotool getactivewindow) | xclip -selection clipboard -target image/png &")
  -- screenshot fullscreen to file
  , ("M-C-3",   spawn $ "maim " ++ bashScreenshotName ++ " &")
  -- screenshot fullscreen to clipboard
  , ("M-C-S-3", spawn "maim | xclip -selection clipboard -target image/png &")
  -- screenshot from selection to file
  , ("M-C-4",   spawn $ "maim --noopengl --select " ++ bashScreenshotName ++ " &")
  -- screenshot from selection to clipboard
  , ("M-C-S-4", spawn "maim --noopengl --select | xclip -selection clipboard -target image/png &")
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
  { modMask            = mod4Mask -- Rebind Mod to the Super key
  , terminal           = myTerminal
  , borderWidth        = 1
  , normalBorderColor  = "#363636"
  , focusedBorderColor = "darkGreen"
  , layoutHook         = myLayout
  , startupHook        = myStartupHook
  , manageHook         = myManageHook -- Match on certain windows
  , handleEventHook    = handleEventHook def <+> fullscreenEventHook
  } `additionalKeysP` myKeys

main :: IO ()
main = xmonad . ewmh =<< statusBar "xmobar" myXmobarPP toggleStrutsKey myConfig
  where
    toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
    toggleStrutsKey XConfig{ modMask = m } = (m, xK_b)
