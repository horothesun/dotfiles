local const = require("modules.constants")

------------------
---- MONITORS ----
------------------

require("modules.monitors_work")

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
local browser = uwsmApp("brave-origin")
local menu = uwsmApp("rofi -show drun -matching prefix")
local statusBar = uwsmApp("waybar")
local restartStatusBar = "pkill waybar || " .. statusBar
local emojis = uwsmApp("rofi -modi emoji -show emoji")
local clipboardManager = uwsmApp("cliphist list | rofi -dmenu | cliphist decode | wl-copy")
local wipeClipboardManagerHistory = "cliphist wipe"
local powerMenu = uwsmApp('"${HOME}/.config/rofi/power_menu.sh"')

local monitorsMenu = uwsmApp('"${HOME}/.config/rofi/monitors_menu.sh"')
local getFocusedMonitorBus = '"${HOME}/bin/get_focused_monitor_i2c_bus.sh"'
local increaseMonitorBrightness = "ddcutil --bus $(" .. getFocusedMonitorBus .. ") setvcp 10 + 5" -- +5%
local decreaseMonitorBrightness = "ddcutil --bus $(" .. getFocusedMonitorBus .. ") setvcp 10 - 5" -- +5%
local increaseMonitorContrast = "ddcutil --bus $(" .. getFocusedMonitorBus .. ") setvcp 12 + 5"   -- +5%
local decreaseMonitorContrast = "ddcutil --bus $(" .. getFocusedMonitorBus .. ") setvcp 12 - 5"   -- +5%

local masterToggle = "amixer set Master toggle"
local masterVolumeDown = "amixer set Master playback 5%-"
local masterVolumeUp = "amixer set Master playback 5%+"

local screenshotFolder = '"${HOME}/Downloads"'
local screenshotFileName = '"Screenshot $(date -u \'+%Y-%m-%d at %H.%M.%S\').png"'
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

hl.on("hyprland.start", function()
  for _, command in ipairs {
    wipeClipboardManagerHistory,
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

hl.env("XCURSOR_SIZE", const.CURSOR_SIZE)
hl.env("HYPRCURSOR_SIZE", const.CURSOR_SIZE)

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

hl.config {
  general    = {
    gaps_in          = 2,
    gaps_out         = 4,
    border_size      = 1,
    col              = {
      active_border = "rgba(306315ff)",
      inactive_border = "rgba(595959aa)"
    },
    resize_on_border = false,
    -- Please see https://wiki.hyprland.org/Configuring/Tearing/ before you turn this on
    allow_tearing    = false,
    layout           = "master"
  },

  decoration = {
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
    blur = {
      enabled = false,
      size = 3,
      passes = 1,
      vibrancy = 0.1696
    }
  },
  animations = {
    enabled = true
  }
}


-- Default curves and animations
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

hl.config { dwindle = { preserve_split = true } }

hl.config { master = { new_status = "slave", mfact = 0.5 } }


----------------
----  MISC  ----
----------------

hl.config {
  misc = {
    force_default_wallpaper  = -1,    -- Set to 0 or 1 to disable the anime mascot wallpapers
    disable_hyprland_logo    = true,  -- If true disables the random hyprland logo / anime girl background. :(
    disable_splash_rendering = true,  -- If true disables the Hyprland splash rendering
    enable_anr_dialog        = false, -- ANR = "App Not Responding"
    -- vfr                     = true  -- lowers the amount of sent frames when nothing is happening on-screen
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

local function super(key) return const.MAIN_MOD .. " + " .. key end
local function shift(key) return const.SHIFT_MOD .. " + " .. key end
local function superShift(key) return const.MAIN_MOD .. " + " .. const.SHIFT_MOD .. " + " .. key end
local function superControl(key) return const.MAIN_MOD .. " + " .. const.CONTROL_MOD .. " + " .. key end
local function superAlt(key) return const.MAIN_MOD .. " + " .. const.ALT_MOD .. " + " .. key end
local function superShiftControl(key)
  return const.MAIN_MOD .. " + " .. const.SHIFT_MOD .. " + " .. const.CONTROL_MOD .. " + " .. key
end
local function superShiftAlt(key)
  return const.MAIN_MOD .. " + " .. const.SHIFT_MOD .. " + " .. const.ALT_MOD .. " + " .. key
end


hl.bind(superShift("M"), hl.dsp.exec_cmd(monitorsMenu))
hl.bind("XF86MonBrightnessDown", hl.dsp.exec_cmd(decreaseMonitorBrightness))
hl.bind("XF86MonBrightnessUp", hl.dsp.exec_cmd(increaseMonitorBrightness))
hl.bind(shift("XF86MonBrightnessDown"), hl.dsp.exec_cmd(decreaseMonitorContrast))
hl.bind(shift("XF86MonBrightnessUp"), hl.dsp.exec_cmd(increaseMonitorContrast))
hl.bind(super("F1"), hl.dsp.exec_cmd(decreaseMonitorBrightness))
hl.bind(super("F2"), hl.dsp.exec_cmd(increaseMonitorBrightness))
hl.bind(superShift("F1"), hl.dsp.exec_cmd(decreaseMonitorContrast))
hl.bind(superShift("F2"), hl.dsp.exec_cmd(increaseMonitorContrast))
hl.bind(super("F10"), hl.dsp.exec_cmd(masterToggle))
hl.bind(super("F11"), hl.dsp.exec_cmd(masterVolumeDown))
hl.bind(super("F12"), hl.dsp.exec_cmd(masterVolumeUp))
hl.bind("XF86AudioMute", hl.dsp.exec_cmd(masterToggle))
hl.bind("XF86AudioLowerVolume", hl.dsp.exec_cmd(masterVolumeDown))
hl.bind("XF86AudioRaiseVolume", hl.dsp.exec_cmd(masterVolumeUp))
hl.bind(super("B"), hl.dsp.exec_cmd(restartStatusBar))
hl.bind(superShift("DELETE"), hl.dsp.exec_cmd(powerMenu))
hl.bind(superShift("RETURN"), hl.dsp.exec_cmd(terminal))
hl.bind(superShift("C"), hl.dsp.window.close())
hl.bind(superShift("W"), hl.dsp.exec_cmd(browser))
hl.bind(superControl("Q"), hl.dsp.exec_cmd(hyprlock))
hl.bind(superShift("Q"), hl.dsp.exit())
hl.bind(super("E"), hl.dsp.exec_cmd(fileManager))
hl.bind(super("V"), hl.dsp.window.float { action = "toggle" })
hl.bind(super("P"), hl.dsp.exec_cmd(menu))
hl.bind(superControl("SPACE"), hl.dsp.exec_cmd(emojis))
hl.bind(superShiftControl("V"), hl.dsp.exec_cmd(clipboardManager))
hl.bind(super("RETURN"), hl.dsp.layout("swapwithmaster master"))
hl.bind(superControl("2"), hl.dsp.exec_cmd(screenshotActiveWindowToFile))
hl.bind(superShiftControl("2"), hl.dsp.exec_cmd(screenshotActiveWindowToClipboard))
hl.bind(superControl("3"), hl.dsp.exec_cmd(screenshotActiveFullScreenToFile))
hl.bind(superShiftControl("3"), hl.dsp.exec_cmd(screenshotActiveFullScreenToClipboard))
hl.bind(superControl("4"), hl.dsp.exec_cmd(screenshotSelectionToFile))
hl.bind(superShiftControl("4"), hl.dsp.exec_cmd(screenshotSelectionToClipboard))
-- hl.bind(super("R"), hl.dsp.window.pseudo()) -- dwindle layout
-- hl.bind(super("J"), hl.dsp.layout("togglesplit")) -- dwindle layout

-- Cycle through windows in current workspace
hl.bind(super("K"), hl.dsp.layout("cycleprev"))
hl.bind(super("J"), hl.dsp.layout("cyclenext"))

for i = 1, 10 do
  local key = i % 10 -- 10 maps to key 0
  hl.bind(super(key), hl.dsp.focus { workspace = i })
  hl.bind(superShift(key), hl.dsp.window.move { workspace = i, follow = false })
end

for key = 1, 5 do
  local workspace = key + 5
  hl.bind(superAlt(key), hl.dsp.focus { workspace = workspace })
  hl.bind(superShiftAlt(key), hl.dsp.window.move { workspace = workspace, follow = false })
end

-- Move/resize windows with mainMod + LMB/RMB and dragging
hl.bind(super("mouse:272"), hl.dsp.window.drag(), { mouse = true })
hl.bind(super("mouse:273"), hl.dsp.window.resize(), { mouse = true })

-- Resize active window with keyboard
local function resizeActiveWindowBind(key, relativeX, relativeY)
  hl.bind(key, hl.dsp.window.resize { x = relativeX, y = relativeY, relative = true }, { repeating = true })
end
resizeActiveWindowBind(superShift("L"), const.WINDOW_RESIZE_STEP, 0)
resizeActiveWindowBind(superShift("H"), -const.WINDOW_RESIZE_STEP, 0)
resizeActiveWindowBind(superShift("K"), 0, -const.WINDOW_RESIZE_STEP)
resizeActiveWindowBind(superShift("J"), 0, const.WINDOW_RESIZE_STEP)


--------------------------------
---- WINDOWS AND WORKSPACES ----
--------------------------------

-- Ignore maximize requests from all apps. You'll probably like this.
local suppressMaximizeRule = hl.window_rule {
  name           = "suppress-maximize-events",
  match          = { class = ".*" },
  suppress_event = "maximize"
}
--suppressMaximizeRule:set_enabled(false)

-- Fix some dragging issues with XWayland
hl.window_rule {
  name     = "fix-xwayland-drags",
  match    = { class = "^$", title = "^$", xwayland = true, float = true, fullscreen = false, pin = false },
  no_focus = true
}

-- unscale XWayland (e.g. Steam, VLC, etc.)
hl.config { xwayland = { force_zero_scaling = true } }

-- hyprshot black border: https://github.com/Gustash/Hyprshot/issues/60#issuecomment-2725250782
local noHyprshotBlackBorderLayerRule = hl.layer_rule {

  name    = "no-hyprshot-black-border",
  match   = { namespace = "selection" },
  no_anim = true
}
--noHyprshotBlackBorderLayerRule:set_enabled(false)

-- primary monitor's workspaces
for workspace = 1, 5 do
  hl.workspace_rule { workspace = "" .. workspace, monitor = const.PRIMARY_MONITOR, layout = "master" }
end

-- secondary monitor's workspaces
for workspace = 6, 10 do
  hl.workspace_rule {
    workspace = "" .. workspace,
    monitor = const.SECONDARY_MONITOR,
    layout = "master",
    layout_opts = { orientation = "top" }
  }
end
