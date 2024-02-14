local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
        "git",
        "clone",
        "--filter=blob:none",
        "https://github.com/folke/lazy.nvim.git",
        "--branch=stable", -- latest stable release
        lazypath,
    })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
    lazy = true,
 --   { 'VonHeikemen/lsp-zero.nvim', branch = 'v2.x' },
    "j-hui/fidget.nvim",
    "folke/lazy.nvim",
    { "folke/neodev.nvim", opts = {  } },
    "nvim-lua/plenary.nvim",
    "nvim-telescope/telescope.nvim",
    "BurntSushi/ripgrep",
    "sharkdp/fd",
    "andweeb/presence.nvim",
    "nvim-treesitter/nvim-treesitter",
    { "catppuccin/nvim", name = "catppuccin" },
    "williamboman/mason.nvim",
    "williamboman/mason-lspconfig.nvim",
    "neovim/nvim-lspconfig",
    'hrsh7th/cmp-nvim-lsp',
    'hrsh7th/cmp-buffer',
    'hrsh7th/cmp-path',
    'hrsh7th/cmp-cmdline',
    'hrsh7th/nvim-cmp',
    'L3MON4D3/LuaSnip',
    'saadparwaiz1/cmp_luasnip',
    { 'folke/zen-mode.nvim', },
    {
        'akinsho/flutter-tools.nvim',
        dependencies = {
            'nvim-lua/plenary.nvim',
            'stevearc/dressing.nvim',
        },
    },
    'tpope/vim-fugitive',

}, { })

require("mason").setup({
    
})

require("mason-lspconfig").setup({
    ensure_installed = { "lua_ls", "rust_analyzer", "clangd", "bashls", "cmake", "pyright" },
})

require('luasnip.loaders.from_vscode').lazy_load()


require("presence").setup({
    editing_text        = "Editing",
    file_explorer_text  = "Browsing",
    git_commit_text     = "Committing changes",
    plugin_manager_text = "Managing plugins",
    reading_text        = "Reading",
    workspace_text      = "Working",
    line_number_text    = " ",
})


require("telescope").setup({
    defaults = {
        prompt_prefix = "> ",
        --        layout_strategy = "vertical",
        layout_config = {
            height = 0.75,
            width = 0.9,
            prompt_position = "top",
        },
        file_ignore_patterns = {
        }
    },

    pickers = {
        layout_config = {
        },
    }
})

require "catppuccin".setup({

    flavour = "frappe",
    transparent_background = false,

})

require('nvim-treesitter.install').compilers = { "gcc","clang" }

require("nvim-treesitter.configs").setup({
    ensure_installed = { "c", "cpp", "bash", "python", "lua", "cmake", "make" },
    highlight = {
        enable = true,
    },
})


require("zen-mode").setup({
    window = {
        backdrop = 0.95,
        width = 150,
        height = 1,
    }
})

require "fidget".setup({
})
