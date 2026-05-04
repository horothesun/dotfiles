-- xx = require("hl")

------------------
---- MONITORS ----
------------------

-- source = ~/.config/hypr/monitors_work.conf

-- 4k x1.5, multi
hl.monitor {
  output = "DP-1",
  mode = "3840x2160@120",
  position = "0x0", -- impacted by scale!
  scale = "1.5",
  vrr = 3,          -- 0: off, 1: on, 2: fullscreen only, 3: fullscreen with video or game content type
  bitdepth = 8
}
hl.monitor {
  output = "HDMI-A-1",
  mode = "3840x2160@60",
  position = "-1440x-560", -- impacted by scale!
  scale = "1.5",
  vrr = 0,                 -- 0: off, 1: on, 2: fullscreen only, 3: fullscreen with video or game content type
  transform = 1,           -- 90 degrees, no flip
  bitdepth = 8
}

-- Fallback rule
hl.monitor {
  output = "",
  mode = "preferred",
  position = "auto",
  scale = "1",
  vrr = 0, -- 0: off, 1: on, 2: fullscreen only, 3: fullscreen with video or game content type
  bitdepth = 8
}


---------------------
---- MY PROGRAMS ----
---------------------

local function uwsmApp(command) return "uwsm app -- " .. command end

local terminal = uwsmApp("alacritty")
local fileManager = uwsmApp("thunar")
local hyprlock = uwsmApp("hyprlock")
local browser = uwsmApp("brave")
local menu = uwsmApp("rofi -show drun")
local statusBar = uwsmApp("waybar")
local restartStatuBar = "pkill waybar || " .. statusBar
local emojis = uwsmApp("rofi -modi emoji -show emoji")

local clipboardManager = uwsmApp("cliphist list | rofi -dmenu | cliphist decode | wl-copy")
local powerMenu = uwsmApp("${HOME}/.config/rofi/power_menu.sh")

local monitorsMenu = uwsmApp("${HOME}/.config/rofi/monitors_menu.sh")
local getFocusedMonitorBus = "${HOME}/bin/get_focused_monitor_i2c_bus.sh"
local increaseMonitorBrightness = 'ddcutil --bus $("' .. getFocusedMonitorBus .. '") setvcp 10 + 5 # +5%'
local decreaseMonitorBrightness = 'ddcutil --bus $("' .. getFocusedMonitorBus .. '") setvcp 10 - 5 # +5%'
local increaseMonitorContrast = 'ddcutil --bus $("' .. getFocusedMonitorBus .. '") setvcp 12 + 5 # +5%'
local decreaseMonitorContrast = 'ddcutil --bus $("' .. getFocusedMonitorBus .. '") setvcp 12 - 5 # +5%'

local masterToggle = "amixer set Master toggle"
local masterVolumeDown = "amixer set Master playback 5%-"
local masterVolumeUp = "amixer set Master playback 5%+"

local screenshotFolder = "${HOME}/Downloads"
local screenshotFileName = "Screenshot $(date -u '+%Y-%m-%d at %H.%M.%S').png"
local screenshotActiveWindowToFile =
    "hyprshot --mode window --mode active --output-folder " .. screenshotFolder .. " --filename " .. screenshotFileName
local screenshotActiveWindowToClipboard = "hyprshot --mode window --mode active --clipboard-only"
local screenshotActiveFullScreenToFile =
    "hyprshot --mode output --mode active --output-folder " .. screenshotFolder .. " --filename " .. screenshotFileName
local screenshotActiveFullScreenToClipboard = "hyprshot --mode output --mode active --clipboard-only"
local screenshotSelectionToFile =
    "hyprshot --mode region --output-folder " .. screenshotFolder .. " --filename " .. screenshotFileName
local screenshotSelectionToClipboard = "hyprshot --mode region --clipboard-only"


-------------------
---- AUTOSTART ----
-------------------

-- See https://wiki.hypr.land/Configuring/Basics/Autostart/

-- Autostart necessary processes (like notifications daemons, status bars, etc.)
-- or execute your favorite apps at launch like this:

hl.on("hyprland.start", function()
  for command in {
    statusBar,

    -- requires hyprpolkitagent package (https://wiki.archlinux.org/title/Polkit#Authentication_agents)
    uwsmApp("/usr/lib/hyprpolkitagent/hyprpolkitagent"),

    -- https://wiki.archlinux.org/title/KDE_Wallet#Unlocking_KWallet_automatically_in_a_window_manager
    uwsmApp("/usr/lib/pam_kwallet_init"),

    -- clipboard management
    uwsmApp("wl-paste --type text --watch cliphist store"),
    uwsmApp("wl-paste --type image --watch cliphist store")
  }
  do hl.exec_cmd(command) end
end)


-------------------------------
---- ENVIRONMENT VARIABLES ----
-------------------------------

-- See https://wiki.hypr.land/Configuring/Advanced-and-Cool/Environment-variables/

hl.env("XCURSOR_SIZE", "24")
hl.env("HYPRCURSOR_SIZE", "24")

-- toolkit-specific scale
--hl.env("GCK_SCALE", "2")

-- to make btrfs-assistant-launcher (and other QT apps) work
hl.env("QT_QPA_PLATFORM", "wayland")

hl.env("LIBVIRT_DEFAULT_URI", "qemu:///system")

-- to improve Steam UI Wayland fractional scaling support
hl.env("SDL_VIDEODRIVER", "wayland")

-- Dark mode switching: https://wiki.archlinux.org/title/Dark_mode_switching
hl.env("GTK_THEME", "Adwaita:dark")
hl.env("GTK2_RC_FILES", "/usr/share/themes/Adwaita-dark/gtk-2.0/gtkrc")
hl.env("QT_STYLE_OVERRIDE", "Adwaita-Dark")
hl.env("QT_QPA_PLATFORMTHEME", "Adwaita:dark")


-----------------------
----- PERMISSIONS -----
-----------------------

-- See https://wiki.hypr.land/Configuring/Advanced-and-Cool/Permissions/
-- Please note permission changes here require a Hyprland restart and are not applied on-the-fly for security reasons

hl.config {
  ecosystem = {
    enforce_permissions = false,
    no_update_news = true
  }
}

-- hl.permission("/usr/(bin|local/bin)/grim", "screencopy", "allow")
-- hl.permission("/usr/(lib|libexec|lib64)/xdg-desktop-portal-hyprland", "screencopy", "allow")
-- hl.permission("/usr/(bin|local/bin)/hyprpm", "plugin", "allow")


-----------------------
---- LOOK AND FEEL ----
-----------------------

-- Refer to https://wiki.hypr.land/Configuring/Basics/Variables/
hl.config {

  general = {
    gaps_in     = 2,
    gaps_out    = 4,

    border_size = 1,

    -- https://wiki.hyprland.org/Configuring/Variables/#variable-types for info about colors
    col         = {
      -- active_border = "rgba(306315ff)",
      active_border = { colors = { "rgba(33ccffee)", "rgba(00ff99ee)", angle = 45 },

        inactive_border = "rgba(595959aa)"
      },

      -- Set to true enable resizing windows by clicking and dragging on borders and gaps
      resize_on_border = false,

      -- Please see https://wiki.hyprland.org/Configuring/Tearing/ before you turn this on
      allow_tearing = false,

      layout = "master"
    },

    -- https://wiki.hyprland.org/Configuring/Variables/#decoration
    decoration  = {
      rounding = 4,
      rounding_power = 2,

      -- Change transparency of focused and unfocused windows
      active_opacity = 1.0,
      inactive_opacity = 1.0,

      shadow = {
        enabled = false,
        range = 4,
        render_power = 3,
        color = "rgba(1a1a1aee)",
      },

      -- https://wiki.hyprland.org/Configuring/Variables/#blur
      blur = {
        enabled = false,
        size = 3,
        passes = 1,

        vibrancy = 0.1696
      }
    },

    -- https://wiki.hyprland.org/Configuring/Variables/#animations
    animations  = {
      enabled = true
    }
  }

}

-- Default curves and animations, see https://wiki.hypr.land/Configuring/Advanced-and-Cool/Animations/
hl.curve("easeOutQuint", { type = "bezier", points = { { 0.23, 1 }, { 0.32, 1 } } })
hl.curve("easeInOutCubic", { type = "bezier", points = { { 0.65, 0.05 }, { 0.36, 1 } } })
hl.curve("linear", { type = "bezier", points = { { 0, 0 }, { 1, 1 } } })
hl.curve("almostLinear", { type = "bezier", points = { { 0.5, 0.5 }, { 0.75, 1 } } })
hl.curve("quick", { type = "bezier", points = { { 0.15, 0 }, { 0.1, 1 } } })

-- Default springs
hl.curve("easy", { type = "spring", mass = 1, stiffness = 71.2633, dampening = 15.8273644 }) -- new

hl.animation({ leaf = "global", enabled = true, speed = 10, bezier = "default" })
hl.animation({ leaf = "border", enabled = true, speed = 5.39, bezier = "easeOutQuint" })
hl.animation({ leaf = "windows", enabled = true, speed = 4.79, spring = "easy" })                       -- "easeOutQuint"
hl.animation({ leaf = "windowsIn", enabled = true, speed = 4.1, spring = "easy", style = "popin 87%" }) -- "easeOutQuint"
hl.animation({ leaf = "windowsOut", enabled = true, speed = 1.49, bezier = "linear", style = "popin 87%" })
hl.animation({ leaf = "fadeIn", enabled = true, speed = 1.73, bezier = "almostLinear" })
hl.animation({ leaf = "fadeOut", enabled = true, speed = 1.46, bezier = "almostLinear" })
hl.animation({ leaf = "fade", enabled = true, speed = 3.03, bezier = "quick" })
hl.animation({ leaf = "layers", enabled = true, speed = 3.81, bezier = "easeOutQuint" })
hl.animation({ leaf = "layersIn", enabled = true, speed = 4, bezier = "easeOutQuint", style = "fade" })
hl.animation({ leaf = "layersOut", enabled = true, speed = 1.5, bezier = "linear", style = "fade" })
hl.animation({ leaf = "fadeLayersIn", enabled = true, speed = 1.79, bezier = "almostLinear" })
hl.animation({ leaf = "fadeLayersOut", enabled = true, speed = 1.39, bezier = "almostLinear" })
hl.animation({ leaf = "workspaces", enabled = true, speed = 1.94, bezier = "almostLinear", style = "fade" })
hl.animation({ leaf = "workspacesIn", enabled = true, speed = 1.21, bezier = "almostLinear", style = "fade" })
hl.animation({ leaf = "workspacesOut", enabled = true, speed = 1.94, bezier = "almostLinear", style = "fade" })
hl.animation({ leaf = "zoomFactor", enabled = true, speed = 7, bezier = "quick" }) -- new

-- See https://wiki.hypr.land/Configuring/Layouts/Dwindle-Layout/ for more
hl.config {
  dwindle = {
    pseudotile = true,    -- Master switch for pseudotiling. Enabling is bound to mainMod + R in the keybinds section below
    preserve_split = true -- You probably want this
  }
}

-- See https://wiki.hypr.land/Configuring/Layouts/Master-Layout/ for more
hl.config {
  master = {
    new_status = "slave",
    mfact = 0.5
  }
}

-- See https://wiki.hypr.land/Configuring/Layouts/Scrolling-Layout/ for more
-- hl.config {
--   scrolling = {
--     fullscreen_on_one_column = true,
--   }
-- }

----------------
----  MISC  ----
----------------

hl.config {
  misc = {
    force_default_wallpaper = -1,   -- Set to 0 or 1 to disable the anime mascot wallpapers
    disable_hyprland_logo   = true, -- If true disables the random hyprland logo / anime girl background. :(
    enable_anr_dialog       = false,

    --vfr = true -- lowers the amount of sent frames when nothing is happening on-screen
  }
}


---------------
---- INPUT ----
---------------

hl.config {
  input = {
    kb_layout = "gb",
    kb_variant = "mac",
    kb_model = "",
    kb_options = "",
    kb_rules = "",
    numlock_by_default = true,
    repeat_rate = 40,
    repeat_delay = 200,

    follow_mouse = true,

    sensitivity = -0.92, -- -1.0 - 1.0, 0 means no modification

    touchpad = {
      natural_scroll = false
    }

  }
}


---------------------
---- KEYBINDINGS ----
---------------------

local mainMod = "SUPER" -- Sets "Windows" key as main modifier

-- Example binds, see https://wiki.hypr.land/Configuring/Basics/Binds/ for more
hl.bind(mainMod .. " + SHIFT + M", hl.dsp.exec_cmd(monitorsMenu))
hl.bind("XF86MonBrightnessDown", hl.dsp.exec_cmd(decreaseMonitorBrightness))
hl.bind("XF86MonBrightnessUp", hl.dsp.exec_cmd(increaseMonitorBrightness))
hl.bind("SHIFT + XF86MonBrightnessDown", hl.dsp.exec_cmd(decreaseMonitorContrast))
hl.bind("SHIFT + XF86MonBrightnessUp", hl.dsp.exec_cmd(increaseMonitorContrast))
hl.bind(mainMod .. " + F1", hl.dsp.exec_cmd(decreaseMonitorBrightness))
hl.bind(mainMod .. " + F2", hl.dsp.exec_cmd(increaseMonitorBrightness))
hl.bind(mainMod .. " + SHIFT + F1", hl.dsp.exec_cmd(decreaseMonitorContrast))
hl.bind(mainMod .. " + SHIFT + F2", hl.dsp.exec_cmd(increaseMonitorContrast))
hl.bind(mainMod .. " + F10", hl.dsp.exec_cmd(masterToggle))
hl.bind(mainMod .. " + F11", hl.dsp.exec_cmd(masterVolumeDown))
hl.bind(mainMod .. " + F12", hl.dsp.exec_cmd(masterVolumeUp))
hl.bind("XF86AudioMute", hl.dsp.exec_cmd(masterToggle))
hl.bind("XF86AudioLowerVolume", hl.dsp.exec_cmd(masterVolumeDown))
hl.bind("XF86AudioRaiseVolume", hl.dsp.exec_cmd(masterVolumeUp))
hl.bind(mainMod .. " + B", hl.dsp.exec_cmd(restartStatuBar))
hl.bind(mainMod .. " + SHIFT + DELETE", hl.dsp.exec_cmd(powerMenu))
hl.bind(mainMod .. " + SHIFT + RETURN", hl.dsp.exec_cmd(terminal))
hl.bind(mainMod .. " + SHIFT + C", hl.dsp.window.kill(hl.get_active_window)) -- killallactive
-- hl.bind($mainMod SHIFT        , W                    , exec          , $browser
-- hl.bind($mainMod CONTROL      , Q                    , exec          , $hyprlock
-- hl.bind($mainMod SHIFT        , Q                    , exit          ,
-- hl.bind($mainMod              , E                    , exec          , $fileManager
-- hl.bind($mainMod              , V                    , togglefloating,
-- hl.bind($mainMod              , P                    , exec          , $menu
-- hl.bind($mainMod CONTROL      , SPACE                , exec          , $emojis
-- hl.bind($mainMod SHIFT CONTROL, V                    , exec          , $clipboardManager
-- hl.bind($mainMod              , RETURN               , layoutmsg     , swapwithmaster master # master layout
-- hl.bind($mainMod CONTROL      , 2                    , exec          , $screenshotActiveWindowToFile
-- hl.bind($mainMod SHIFT CONTROL, 2                    , exec          , $screenshotActiveWindowToClipboard
-- hl.bind($mainMod CONTROL      , 3                    , exec          , $screenshotActiveFullScreenToFile
-- hl.bind($mainMod SHIFT CONTROL, 3                    , exec          , $screenshotActiveFullScreenToClipboard
-- hl.bind($mainMod CONTROL      , 4                    , exec          , $screenshotSelectionToFile
-- hl.bind($mainMod SHIFT CONTROL, 4                    , exec          , $screenshotSelectionToClipboard
-- #bind = $mainMod              , R                   , pseudo        , # dwindle layout
-- #bind = $mainMod              , J                   , togglesplit   , # dwindle layout

-- Cycle through windows in current workspace
--bind = $mainMod, K, layoutmsg, cycleprev
--bind = $mainMod, J, layoutmsg, cyclenext

-- Switch workspaces with mainMod + [0-9]
--bind = $mainMod, 1, workspace, 1
--bind = $mainMod, 2, workspace, 2
--bind = $mainMod, 3, workspace, 3
--bind = $mainMod, 4, workspace, 4
--bind = $mainMod, 5, workspace, 5

--bind = $mainMod, 6, workspace, 6
--bind = $mainMod, 7, workspace, 7
--bind = $mainMod, 8, workspace, 8
--bind = $mainMod, 9, workspace, 9
--bind = $mainMod, 0, workspace, 10

--bind = $mainMod ALT, 1, workspace, 6
--bind = $mainMod ALT, 2, workspace, 7
--bind = $mainMod ALT, 3, workspace, 8
--bind = $mainMod ALT, 4, workspace, 9
--bind = $mainMod ALT, 5, workspace, 10

-- AltGr = Mod5
--bind = $mainMod Mod5, 1, workspace, 6
--bind = $mainMod Mod5, 2, workspace, 7
--bind = $mainMod Mod5, 3, workspace, 8
--bind = $mainMod Mod5, 4, workspace, 9
--bind = $mainMod Mod5, 5, workspace, 10

-- Move active window to a workspace with mainMod + SHIFT + [0-9]
--bind = $mainMod SHIFT, 1, movetoworkspacesilent, 1
--bind = $mainMod SHIFT, 2, movetoworkspacesilent, 2
--bind = $mainMod SHIFT, 3, movetoworkspacesilent, 3
--bind = $mainMod SHIFT, 4, movetoworkspacesilent, 4
--bind = $mainMod SHIFT, 5, movetoworkspacesilent, 5

--bind = $mainMod SHIFT, 6, movetoworkspacesilent, 6
--bind = $mainMod SHIFT, 7, movetoworkspacesilent, 7
--bind = $mainMod SHIFT, 8, movetoworkspacesilent, 8
--bind = $mainMod SHIFT, 9, movetoworkspacesilent, 9
--bind = $mainMod SHIFT, 0, movetoworkspacesilent, 10

--bind = $mainMod ALT SHIFT, 1, movetoworkspacesilent, 6
--bind = $mainMod ALT SHIFT, 2, movetoworkspacesilent, 7
--bind = $mainMod ALT SHIFT, 3, movetoworkspacesilent, 8
--bind = $mainMod ALT SHIFT, 4, movetoworkspacesilent, 9
--bind = $mainMod ALT SHIFT, 5, movetoworkspacesilent, 10

-- AltGr = Mod5
--bind = $mainMod Mod5 SHIFT, 1, movetoworkspacesilent, 6
--bind = $mainMod Mod5 SHIFT, 2, movetoworkspacesilent, 7
--bind = $mainMod Mod5 SHIFT, 3, movetoworkspacesilent, 8
--bind = $mainMod Mod5 SHIFT, 4, movetoworkspacesilent, 9
--bind = $mainMod Mod5 SHIFT, 5, movetoworkspacesilent, 10

-- Example special workspace (scratchpad)
--#bind = $mainMod, S, togglespecialworkspace, magic
--#bind = $mainMod SHIFT, S, movetoworkspace, special:magic

-- Move/resize windows with mainMod + LMB/RMB and dragging
--bindm = $mainMod, mouse:272, movewindow
--bindm = $mainMod, mouse:273, resizewindow

-- Resize active window with keyboard
--bindel = $mainMod SHIFT, L, resizeactive,  125   0
--bindel = $mainMod SHIFT, H, resizeactive, -125   0
--bindel = $mainMod SHIFT, K, resizeactive,   0 -125
--bindel = $mainMod SHIFT, J, resizeactive,   0  125

-- Move active window with keyboard
--bindel = $mainMod, L, moveactive,  100    0
--bindel = $mainMod, H, moveactive, -100    0
--bindel = $mainMod, K, moveactive,    0 -100
--bindel = $mainMod, J, moveactive,    0  100
