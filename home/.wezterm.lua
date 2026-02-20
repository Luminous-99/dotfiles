local wezterm = require("wezterm")
local config = wezterm.config_builder()

config.color_scheme = 'Gruvbox (Gogh)'
config.font = wezterm.font("0xProto Nerd Font", { weight = "Regular", italic = false })
config.font_size = 12
config.enable_tab_bar = false
config.freetype_load_flags = 'NO_HINTING'

return config
