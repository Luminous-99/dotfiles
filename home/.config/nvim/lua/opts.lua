local map = vim.api.nvim_set_keymap
local nvim = vim.api

vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.termguicolors = true
vim.opt.encoding = "utf-8"
vim.opt.fileencoding = "utf-8"
vim.g.mapleader = " "
vim.g.blamer_delay = 500
vim.g.blamer_enabled = true
vim.wo.cursorline = true;

vim.cmd.colorscheme("gruvbox")
vim.cmd("set background=light")

map("n", "<Leader>ff", "<cmd>Telescope find_files<cr>", { noremap = true, silent = true })
map("n", "<Leader>fg", "<cmd>Telescope live_grep<cr>", { noremap = true, silent = true })
map("n", "<Leader>fb", "<cmd>Telescope buffers<cr>", { noremap = true, silent = true })
map("n", "<Leader>fh", "<cmd>Telescope help_tags<cr>", { noremap = true, silent = true })
map("n", "<Leader>sh", "<cmd>ClangdSwitchSourceHeader<cr>", {})
map("n", "<Leader>dn", "<cmd>lua vim.diagnostic.goto_next()<cr>", {})
map("n", "<Leader>dp", "<cmd>lua vim.diagnostic.goto_prev()<cr>", {})
map("n", "<C-d>", "<C-d>zz", {})
map("n", "<C-u>", "<C-u>zz", {})
map("i", "<C-c>", "<Esc>", {})

local comment_prefixes = {
    { "*.c",      "//" },
    { "*.cpp",    "//" },
    { "*.cs",     "//" },
    { "*.rs",     "//" },
    { "*.java",   "//" },
    { "*.js",     "//" },
    { "*.ts",     "//" },
    { "*.php",    "//" },
    { "*.sh",     "#" },
    { "Makefile", "#" },
    { "*.lua",    "--" },
    { "*.lisp",   ";;" },
    { "*.el",     ";;" },
    { ".emacs",   ";;" },
    { "*.scm",    ";;" },
}

for _, pair in ipairs(comment_prefixes) do
    local ext, pre = unpack(pair)
    vim.api.nvim_create_autocmd("BufEnter", {
        pattern = ext,
        callback = function()
            map("n", "<Leader>cc", "_Di" .. pre .. " <Esc>p", {})
        end
    })
end

local containter_syms = {
    { '"',  '"',  '"' },
    { '\'', '\'', '\'' },
    { 'b',  '(',  ')' },
    { '(',  '(',  ')' },
    { 'B',  '{',  '}' },
    { '{',  '{',  '}' },
    { 'q',  '[',  ']' },
    { '[',  '[',  ']' },
}

for _, triple in ipairs(containter_syms) do
    local key, left, right = unpack(triple)
    map("n", '<Leader>w' .. key, 'diwi' .. left .. right .. '<Esc>hpF' .. left, {})
end

local diagnostic_signs = { Error = "", Warn = "", Info = "󰋼", Hint = "󰋼" }

for name, sign in pairs(diagnostic_signs) do
    local hl = "DiagnosticSign" .. name
    vim.fn.sign_define(hl, {
        text   = sign,
        numhl  = hl,
        texthl = hl,
    })
end
