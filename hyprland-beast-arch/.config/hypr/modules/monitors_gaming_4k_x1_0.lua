local const = require("modules.constants")

hl.monitor {
  output = const.PRIMARY_MONITOR,
  mode = "3840x2160@120",
  position = "0x0",
  scale = "1",
  vrr = 3, -- 0: off, 1: on, 2: fullscreen only, 3: fullscreen with video or game content type
  bitdepth = 8
}
hl.monitor {
  output = const.SECONDARY_MONITOR,
  disabled = true
}
