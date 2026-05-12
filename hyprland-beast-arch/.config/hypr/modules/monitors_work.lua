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
