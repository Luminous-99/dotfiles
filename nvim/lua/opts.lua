local opt = vim.opt
local map = vim.api.nvim_set_keymap

opt.number = true
opt.relativenumber = true
opt.tabstop=4
opt.shiftwidth=4
opt.expandtab = true
opt.termguicolors = true
opt.encoding= "utf-8"
opt.fileencoding= "utf-8"

vim.cmd("colorscheme catppuccin")

--vim.api.nvim_set_hl(0,"Normal",     { bg = "none" })
--vim.api.nvim_set_hl(0,"NormalFloat",{ bg = "none" })
--vim.api.nvim_set_hl(0,"LineNr",{ fg = "#5FF05F",bg = "none" })

map("n","<Space>ff",":Telescope find_files<cr>",{ noremap = true, silent = true })
map("n","<Space>fg",":Telescope live_grep<cr>",{ noremap = true, silent = true })
map("n","<Space>fb",":Telescope buffers<cr>",{ noremap = true, silent = true })
map("n","<Space>fh",":Telescope help_tags<cr>",{ noremap = true, silent = true })
map("n","<C-d>","<C-d>zz", {})
map("n","<C-u>","<C-u>zz", {})
map("i","<C-c>","<Esc>", {})
