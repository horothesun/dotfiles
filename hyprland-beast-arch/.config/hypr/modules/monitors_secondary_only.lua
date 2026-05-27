local const = require("modules.constants")

hl.monitor {
  output = const.PRIMARY_MONITOR,
  disabled = true
}
hl.monitor {
  output = const.SECONDARY_MONITOR,
  mode = "3840x2160@60",
  position = "0x0",
  scale = "1.5",
  vrr = 0,       -- 0: off, 1: on, 2: fullscreen only, 3: fullscreen with video or game content type
  transform = 1, -- 90 degrees, no flip
  bitdepth = 8
}
