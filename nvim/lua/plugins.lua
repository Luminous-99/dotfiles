require("packer").startup(function(use)
    use "wbthomason/packer.nvim"
    use "nvim-lua/plenary.nvim"
    use "nvim-telescope/telescope.nvim"
    use "BurntSushi/ripgrep"
    use "sharkdp/fd"
    use "andweeb/presence.nvim"
    use "nvim-treesitter/nvim-treesitter"
    use "folke/tokyonight.nvim"
    use "williamboman/mason.nvim"
    use "williamboman/mason-lspconfig.nvim"
    use "neovim/nvim-lspconfig"
    use 'hrsh7th/cmp-nvim-lsp'
    use 'hrsh7th/cmp-buffer'
    use 'hrsh7th/cmp-path'
    use 'hrsh7th/cmp-cmdline'
    use 'hrsh7th/nvim-cmp'
    use 'L3MON4D3/LuaSnip'
    use 'saadparwaiz1/cmp_luasnip'
    use { 'VonHeikemen/lsp-zero.nvim',
        branch = 'v2.x' }
    use { "folke/neodev.nvim" , opts = {} }
    use 'tpope/vim-fugitive'
end)

require("mason").setup()
require("mason-lspconfig").setup()


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
        prompt_prefix = "Enter>",
        layout_strategy = "vertical",
        layout_config = {
            height = 0.75,
            width = 0.5,
            prompt_position = "top",
            preview_height = 0
        },
        file_ignore_patterns = {
            "node_modules",
            ".git",
            "vendor",
            "CMakeFiles",
        }
    },

    pickers = {
        layout_config = {
        },
    }
})

local tokyonight = require"tokyonight"

tokyonight.setup({

    transparent = true,
    styles = {

        comments = { italic = true },
        functions = { italic = true },
        keywords = { italic = true },
        variables = {  },
        sidebars = "transparent",
        floats = "transparent",
    }
})

require('nvim-treesitter.install').compilers = { 'clang' }

require("nvim-treesitter.configs").setup({
    highlight = {
        enable = true
    }
})


