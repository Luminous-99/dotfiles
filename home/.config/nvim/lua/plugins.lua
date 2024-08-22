local lazypath = vim.fn.stdpath('data') .. '/lazy/lazy.nvim'
local plugpath = vim.fn.stdpath('data') .. '/lazy'

if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
        'git',
        'clone',
        '--filter=blob:none',
        'https://github.com/folke/lazy.nvim.git',
        '--branch=stable', -- latest stable release
        lazypath,
    })
end
vim.opt.rtp:prepend(lazypath)

require('lazy').setup({
    lazy = true,
    'j-hui/fidget.nvim',
    'folke/lazy.nvim',
    { 'folke/neodev.nvim', opts = {} },
    'nvim-lua/plenary.nvim',
    'nvim-telescope/telescope.nvim',
    'BurntSushi/ripgrep',
    'sharkdp/fd',
    'andweeb/presence.nvim',
    'nvim-treesitter/nvim-treesitter',
    'williamboman/mason.nvim',
    'williamboman/mason-lspconfig.nvim',
    'neovim/nvim-lspconfig',
    'hrsh7th/cmp-nvim-lsp',
    'hrsh7th/cmp-buffer',
    {
        'VonHeikemen/lsp-zero.nvim',
        branch = 'v2.x',
        lazy = true,
        config = function()
            require('lsp-zero.settings').preset({})
        end
    },
    'hrsh7th/cmp-path',
    'hrsh7th/cmp-cmdline',
    'hrsh7th/nvim-cmp',
    'L3MON4D3/LuaSnip',
    'saadparwaiz1/cmp_luasnip',
    'folke/zen-mode.nvim',
    'tpope/vim-fugitive',
    'p00f/clangd_extensions.nvim',
    'APZelos/blamer.nvim',
    { 'catppuccin/nvim',   name = 'catppuccin' },
    'morhetz/gruvbox',
    'kovisoft/slimv',
    "nvim-treesitter/nvim-treesitter-textobjects",
    {
        'nvim-neorg/neorg',
        version = '*',
        config = true,
    },
}, {})

vim.opt.runtimepath:append(plugpath .. '/vlime/vim')

require('neorg').setup({})
require('mason').setup({})
require('fidget').setup({})
require('luasnip.loaders.from_vscode').lazy_load()

require('mason-lspconfig').setup({
    ensure_installed = { 'lua_ls', 'rust_analyzer', 'clangd', 'bashls', 'cmake', 'pyright' },
})

require('presence').setup({
    enable_line_number = true
})

require('telescope').setup({
    defaults = {
        prompt_prefix = '> ',
        layout_strategy = 'horizontal',
        layout_config = {
            height = 0.75,
            width = 0.9,
            prompt_position = 'top',
        },
    },
})

require('catppuccin').setup({
    flavour = 'frappe',
    transparent_background = false,
})

require 'nvim-treesitter.configs'.setup {
}

require('nvim-treesitter.configs').setup({
    modules = {},
    ignore_install = {},
    sync_install = false,
    auto_install = false,
    ensure_installed = { 'c', 'cpp', 'commonlisp', 'bash', 'python', 'lua', 'cmake', 'make' },
    highlight = {
        enable = true,

    },
    textobjects = {
        select = {
            enable = true,

            -- Automatically jump forward to textobj, similar to targets.vim
            lookahead = true,

            keymaps = {
                -- You can use the capture groups defined in textobjects.scm
                ["af"] = "@function.outer",
                ["if"] = "@function.inner",
                ["il"] = "@loop.inner",
                ["al"] = "@loop.outer",
                ["iC"] = "@conditional.inner",
                ["aC"] = "@conditional.outer",
                -- You can optionally set descriptions to the mappings (used in the desc parameter of
                -- nvim_buf_set_keymap) which plugins like which-key display
                ["ic"] = { query = "@class.inner", desc = "Select inner part of a class region" },
                ["ac"] = "@class.outer",
                -- You can also use captures from other query groups like `locals.scm`
                ["as"] = { query = "@scope", query_group = "locals", desc = "Select language scope" },
            },
            -- You can choose the select mode (default is charwise 'v')
            --
            -- Can also be a function which gets passed a table with the keys
            -- * query_string: eg '@function.inner'
            -- * method: eg 'v' or 'o'
            -- and should return the mode ('v', 'V', or '<c-v>') or a table
            -- mapping query_strings to modes.
            selection_modes = {
                ['@parameter.outer'] = 'v', -- charwise
                ['@function.outer'] = 'V', -- linewise
                ['@class.outer'] = '<c-v>', -- blockwise
            },
            -- If you set this to `true` (default is `false`) then any textobject is
            -- extended to include preceding or succeeding whitespace. Succeeding
            -- whitespace has priority in order to act similarly to eg the built-in
            -- `ap`.
            --
            -- Can also be a function which gets passed a table with the keys
            -- * query_string: eg '@function.inner'
            -- * selection_mode: eg 'v'
            -- and should return true or false
            include_surrounding_whitespace = true,
        },
    },
})

require('clangd_extensions').setup({
    inlay_hints = {
        inline = false,
    }
});
