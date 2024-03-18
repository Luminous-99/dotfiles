local opt = vim.opt
local map = vim.api.nvim_set_keymap

opt.number = true
opt.relativenumber = true
opt.tabstop = 4
opt.shiftwidth = 4
opt.expandtab = true
opt.termguicolors = true
opt.encoding = "utf-8"
opt.fileencoding = "utf-8"

vim.cmd("colorscheme catppuccin-frappe")

map("n", "<Space>ff", "<cmd>Telescope find_files<cr>", { noremap = true, silent = true })
map("n", "<Space>fg", "<cmd>Telescope live_grep<cr>", { noremap = true, silent = true })
map("n", "<Space>fb", "<cmd>Telescope buffers<cr>", { noremap = true, silent = true })
map("n", "<Space>fh", "<cmd>Telescope help_tags<cr>", { noremap = true, silent = true })
map("n", "<C-d>", "<C-d>zz", {})
map("n", "<C-u>", "<C-u>zz", {})
map("n", "<Space>dn", "<cmd>lua vim.diagnostic.goto_next()<cr>", {})
map("n", "<Space>dp", "<cmd>lua vim.diagnostic.goto_prev()<cr>", {})
map("i", "<C-c>", "<Esc>", {})

local diagnostic_signs = { Error = "", Warn = "", Info = "󰋼", Hint = "󰋼" }

for name, sign in pairs(diagnostic_signs) do
    local hl = "DiagnosticSign" .. name
    vim.fn.sign_define(hl, {
        text   = sign,
        numhl  = hl,
        texthl = hl,
    })
end

vim.api.nvim_create_autocmd('BufEnter', {
    pattern = { '*.qml' },
    callback = function()
        vim.treesitter.start(vim.api.nvim_get_current_buf(), "qmljs");
    end
})
