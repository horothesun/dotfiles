import XMonad

import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Renamed
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

externalDisplay1SleepMultiplier = 0.1 -- check https://www.ddcutil.com/faq/ "ddcutil is slow." section
externalDisplay1Bus = 2 -- get it from `ddcutil detect` -> Display 1 -> I2C bus -> /dev/i2c-<BUS#>
setExternalDisplay1CommandPrefix = "ddcutil" ++
  " --sleep-multiplier " ++ show externalDisplay1SleepMultiplier ++
  " --bus " ++ show externalDisplay1Bus ++ " setvcp"
setExternalDisplay1BrightnessCommandPrefix = setExternalDisplay1CommandPrefix ++ " 10"
setExternalDisplay1ContrastCommandPrefix   = setExternalDisplay1CommandPrefix ++ " 12"

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

myLayout = tall ||| wide ||| full
  where
    tall    = renamed [Replace "\xec00  Tall"] $ Tall nmaster delta ratio
    wide    = renamed [Replace "\xec01  Wide"] $ Mirror tall
    full    = renamed [Replace "\xf50c  Full"] Full
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

myManageHook :: ManageHook
myManageHook = insertPosition Below Newer <> composeAll
  [ className =? "Gimp" --> doFloat
  , isDialog            --> doFloat
  ]

myKeys :: [(String, X ())]
myKeys =
  [ ("<XF86MonBrightnessUp>",       spawn $ "brightnessctl --quiet --device " ++ internalMonitorBacklightDeviceName ++ " set 2%+")
  , ("<XF86MonBrightnessDown>",     spawn $ "brightnessctl --quiet --device " ++ internalMonitorBacklightDeviceName ++ " set 2%-")
  , ("S-<XF86MonBrightnessUp>",     spawn $ setExternalDisplay1BrightnessCommandPrefix ++ " + 10")
  , ("S-<XF86MonBrightnessDown>",   spawn $ setExternalDisplay1BrightnessCommandPrefix ++ " - 10")
  , ("C-S-<XF86MonBrightnessUp>",   spawn $ setExternalDisplay1ContrastCommandPrefix ++ " + 5")
  , ("C-S-<XF86MonBrightnessDown>", spawn $ setExternalDisplay1ContrastCommandPrefix ++ " - 5")
  , ("<XF86KbdBrightnessUp>",       spawn $ "brightnessctl --quiet --device " ++ internalKeyboardBacklightDeviceName ++ " set 2%+")
  , ("<XF86KbdBrightnessDown>",     spawn $ "brightnessctl --quiet --device " ++ internalKeyboardBacklightDeviceName ++ " set 2%-")
  , ("<XF86AudioMute>",             spawn $ "pactl set-sink-mute @DEFAULT_SINK@ toggle")
  , ("<XF86AudioRaiseVolume>",      spawn $ "pactl set-sink-volume @DEFAULT_SINK@ +5%")
  , ("<XF86AudioLowerVolume>",      spawn $ "pactl set-sink-volume @DEFAULT_SINK@ -5%")
  , ("<XF86PowerOff>",              spawn $ "systemctl suspend")
  , ("M-<XF86PowerOff>",            spawn $ "systemctl poweroff")
  , ("M-b",                         sendMessage ToggleStruts) -- toggle status bar for dynamic setup
  -- custom dmenu
  , ("M-p",     spawn dmenuCommand)
  -- launch browser
  , ("M-S-w",   spawn "brave &")
  -- screenshot from current window to file
  , ("M-C-2",   spawn $ "maim --hidecursor --window $(xdotool getactivewindow) " ++ bashScreenshotName ++ " &")
  -- screenshot from current window to clipboard
  , ("M-C-S-2", spawn "maim --hidecursor --window $(xdotool getactivewindow) | xclip -selection clipboard -target image/png &")
  -- screenshot fullscreen to file
  , ("M-C-3",   spawn $ "maim --hidecursor " ++ bashScreenshotName ++ " &")
  -- screenshot fullscreen to clipboard
  , ("M-C-S-3", spawn "maim --hidecursor | xclip -selection clipboard -target image/png &")
  -- screenshot from selection to file
  , ("M-C-4",   spawn $ "maim --hidecursor --noopengl --select " ++ bashScreenshotName ++ " &")
  -- screenshot from selection to clipboard
  , ("M-C-S-4", spawn "maim --hidecursor --noopengl --select | xclip -selection clipboard -target image/png &")
  ]

myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings x = M.union (mouseBindings def x) mappings
  where
    mappings :: M.Map (KeyMask, Button) (Window -> X ())
    mappings = M.fromList [
      -- Super + Right-click: window resize from bottom-rigth corner
        ((4, button3), (\w -> focus w >> Flex.mouseResizeWindow w))
      ]

myXmobarPP :: PP
myXmobarPP = def
  { ppSep             = magenta " | "
  , ppTitleSanitize   = xmobarStrip
  , ppCurrent         = wrap (blue "[") (blue "]")
  , ppVisible         = wrap (lowWhite "(") (lowWhite ")")
  , ppHidden          = white . wrap "" "•"
  , ppHiddenNoWindows = lowWhite . wrap "" ""
  , ppUrgent          = red . wrap (yellow "!") (yellow "!")
  } where
    -- Windows should have *some* title, which should not exceed a sane length
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "<untitled>" else w) . shorten 24
    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

mainScreenOnlySBConfig :: StatusBarConfig
mainScreenOnlySBConfig = statusBarProp "xmobar" (pure myXmobarPP)

multiScreenDynamicSBConfig :: ScreenId -> StatusBarConfig
multiScreenDynamicSBConfig (S sId) = statusBarPropTo prop cmd (pure myXmobarPP)
  where
    prop = "_XMONAD_LOG_" ++ show (1 + sId)
    cmd  = "xmobar -x " ++ show sId ++ " \"${HOME}/xmobarrc_" ++ show sId ++ "\""

barSpawner :: ScreenId -> IO StatusBarConfig
barSpawner (S sId) | sId <= 1 = pure $ multiScreenDynamicSBConfig $ S sId
barSpawner _                  = mempty

mainScreenOnlySB :: LayoutClass l Window => XConfig l -> XConfig (ModifiedLayout AvoidStruts l)
mainScreenOnlySB = withEasySB mainScreenOnlySBConfig defToggleStrutsKey

multiScreenDynamicSBs :: LayoutClass l Window => XConfig l -> XConfig (ModifiedLayout AvoidStruts l)
multiScreenDynamicSBs = dynamicEasySBs barSpawner

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
  , mouseBindings      = myMouseBindings
  } `additionalKeysP` myKeys

main :: IO ()
-- status bar: use either `mainScreenOnlySB` or `multiScreenDynamicSBs`
main = xmonad . ewmhFullscreen . ewmh . multiScreenDynamicSBs $ myConfig
