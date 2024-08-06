import XMonad

import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.ThreeColumns
import XMonad.Util.EZConfig
import XMonad.Util.EZConfig (additionalKeysP)
import qualified XMonad.Util.Hacks as Hacks
import XMonad.Util.Cursor
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce
import XMonad.Util.Ungrab

myTerminal = "alacritty"

internalMonitorBacklightDeviceName = "intel_backlight"
internalKeyboardBacklightDeviceName = "smc::kbd_backlight"
internalMonitorSetResolutionCommand = "xinternal_only.sh"
setKeyboardRepeatDelayAndRateCommand = "xset r rate 280 40"
disableTouchpadTapToClick = "synclient MaxTapTime=0 &"
launchSystemTray = "trayer -l --edge top --align right --widthtype request --padding 5 " ++
  "--SetDockType true --SetPartialStrut true --expand true --monitor primary " ++
  "--transparent true --alpha 0 --tint 0x202020 --height 26 --iconspacing 4 &"
launchNetworkApplet = "nm-applet &"
launchVolumeApplet = "volumeicon &"
launchClipboardManager = "copyq &"
dmenuCommand = "dmenu_run -i -fn \"JetBrainsMono Nerd Font:pixelsize=16:antialias=true:hinting=true\"" ++
  " -nb \"#3c3836\" -nf \"#fbf1c7\" -sb \"#d65d0e\" -sf \"white\" &"

bashScreenshotName = "\"${HOME}/Downloads/Screenshot $(date -u \"+%Y-%m-%d at %H.%M.%S\").png\""

myLayout = tiled ||| Mirror tiled ||| Full
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1     -- Default number of windows in the master pane
    ratio   = 1/2   -- Default proportion of screen occupied by master pane
    delta   = 5/100 -- Percent of screen to increment by when resizing panes

myStartupHook :: X ()
myStartupHook = do
  setDefaultCursor xC_arrow
  spawnOnce internalMonitorSetResolutionCommand
  spawnOnce setKeyboardRepeatDelayAndRateCommand
  spawnOnce disableTouchpadTapToClick
  spawnOnce launchSystemTray
  spawnOnce launchNetworkApplet
  spawnOnce launchVolumeApplet
  spawnOnce launchClipboardManager
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
  , ("<XF86KbdBrightnessUp>",   spawn $ "brightnessctl --quiet --device " ++ internalKeyboardBacklightDeviceName ++ " set 2%+")
  , ("<XF86KbdBrightnessDown>", spawn $ "brightnessctl --quiet --device " ++ internalKeyboardBacklightDeviceName ++ " set 2%-")
  , ("<XF86AudioMute>",         spawn $ "pactl set-sink-mute @DEFAULT_SINK@ toggle")
  , ("<XF86AudioRaiseVolume>",  spawn $ "pactl set-sink-volume @DEFAULT_SINK@ +5%")
  , ("<XF86AudioLowerVolume>",  spawn $ "pactl set-sink-volume @DEFAULT_SINK@ -5%")
  , ("<XF86PowerOff>",          spawn $ "systemctl suspend")
  , ("M-<XF86PowerOff>",        spawn $ "systemctl poweroff")
  -- custom dmenu
  , ("M-p",     spawn dmenuCommand)
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

-- Super + Right-click: window resize from bottom-rigth corner
myMouse x = [
    ((4, button3), (\w -> focus w >> Flex.mouseResizeWindow w))
  ]
newMouse x = M.union (mouseBindings def x) (M.fromList (myMouse x))

myXmobarPP :: PP
myXmobarPP = def
  { ppSep             = magenta " | "
  , ppTitleSanitize   = xmobarStrip
  , ppCurrent         = wrap (blue "[") (blue "]")
  , ppHidden          = white . wrap "" "•"
  , ppHiddenNoWindows = lowWhite . wrap "" ""
  , ppUrgent          = red . wrap (yellow "!") (yellow "!")
  } where
    -- Windows should have *some* title, which should not exceed a sane length
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 24
    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

myStatusBar :: StatusBarConfig
myStatusBar = statusBarProp "xmobar" (pure myXmobarPP)

myConfig = def
  { modMask            = mod4Mask -- Rebind Mod to the Super key
  , terminal           = myTerminal
  , borderWidth        = 1
  , normalBorderColor  = "#363636"
  , focusedBorderColor = "darkGreen"
  , layoutHook         = myLayout
  , startupHook        = myStartupHook
  , manageHook         = myManageHook -- Match on certain windows
  , handleEventHook    = handleEventHook def <+> Hacks.trayerAboveXmobarEventHook
  , mouseBindings      = newMouse
  } `additionalKeysP` myKeys

main :: IO ()
main = xmonad . ewmhFullscreen . ewmh . withEasySB myStatusBar defToggleStrutsKey $ myConfig
