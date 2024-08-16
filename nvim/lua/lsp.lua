local capabilities = require('cmp_nvim_lsp').default_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true;

vim.api.nvim_set_hl(0, 'FloatBorder', { fg = vim.api.nvim_get_hl(0, { name = "Function" }).fg, })

local border = {
    { "╭", "FloatBorder" },
    { "─", "FloatBorder" },
    { "╮", "FloatBorder" },
    { "│", "FloatBorder" },
    { "╯", "FloatBorder" },
    { "─", "FloatBorder" },
    { "╰", "FloatBorder" },
    { "│", "FloatBorder" },
}

local handlers = {
    ["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, { border = border }),
    ["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, { border = border }),
}

vim.api.nvim_create_autocmd('LspAttach', {
    group = vim.api.nvim_create_augroup('UserLspConfig', {}),
    callback = function(ev)
        local opts = { buffer = ev.buf }
        vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
        vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
        vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
        vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
        vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, opts)
        vim.keymap.set('i', '<C-k>', vim.lsp.buf.signature_help, opts)
        vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, opts)
        vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, opts)
        vim.keymap.set('n', '<space>wl', function()
            print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
        end, opts)
        vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, opts)
        vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, opts)
        vim.keymap.set({ 'n', 'v' }, '<space>ca', vim.lsp.buf.code_action, opts)
        vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
        vim.keymap.set('n', '<space>rf', function()
            vim.lsp.buf.format { async = true, }
        end, opts)
    end,
})

local lsp_config = require 'lspconfig'

local lspz = require 'lsp-zero'

lspz.preset('recommended')

lspz.set_preferences({ sign_icons = {}, })

lspz.setup()

require "cmpconf"

lsp_config.clangd.setup({
    cmd = { 'clangd',
        '--header-insertion=never',
        '--completion-style=detailed',
        '--enable-config',
        '--function-arg-placeholders=0',
        '--cross-file-rename',
        '--background-index'
    },
    handlers = handlers,
    on_attach = function()
        require("clangd_extensions.inlay_hints").setup_autocmd()
        require("clangd_extensions.inlay_hints").set_inlay_hints()
    end
})

lsp_config.ruby_lsp.setup({
    handlers = handlers,
    capabilities = capabilities,
})

lsp_config.elixirls.setup({
    handlers = handlers,
    capabilities = capabilities,
})

lsp_config.gopls.setup({
    handlers = handlers,
    capabilities = capabilities,
})

lsp_config.lua_ls.setup {
    settings = {
        Lua = {
            workspace = {
                checkThirdParty = false,
            }
        }
    },
    handlers = handlers,
    capabilities = capabilities,
}

vim.api.nvim_create_autocmd('FileType', {
    pattern = 'sh',
    callback = function()
        vim.lsp.start({
            name = 'bash-language-server',
            cmd  = { 'bash-language-server', 'start' },
        })
    end,
})

lsp_config.omnisharp.setup({
    cmd = { 'dotnet', vim.fn.expandcmd('~/.local/share/nvim/mason/packages/omnisharp/libexec/OmniSharp.dll') },

    -- Enables support for reading code style, naming convention and analyzer
    -- settings from .editorconfig.
    enable_editorconfig_support = true,

    -- If true, MSBuild project system will only load projects for files that
    -- were opened in the editor. This setting is useful for big C# codebases
    -- and allows for faster initialization of code navigation features only
    -- for projects that are relevant to code that is being edited. With this
    -- setting enabled OmniSharp may load fewer projects and may thus display
    -- incomplete reference lists for symbols.
    enable_ms_build_load_projects_on_demand = false,

    -- Enables support for roslyn analyzers, code fixes and rulesets.
    enable_roslyn_analyzers = false,

    -- Specifies whether 'using' directives should be grouped and sorted during
    -- document formatting.
    organize_imports_on_format = true,

    -- Enables support for showing unimported types and unimported extension
    -- methods in completion lists. When committed, the appropriate using
    -- directive will be added at the top of the current file. This option can
    -- have a negative impact on initial completion responsiveness,
    -- particularly for the first few completion sessions after opening a
    -- solution.
    enable_import_completion = true,

    -- Specifies whether to include preview versions of the .NET SDK when
    -- determining which version to use for project loading.
    sdk_include_prereleases = true,

    -- Only run analyzers against open files when 'enableRoslynAnalyzers' is
    -- true
    analyze_open_documents_only = false,
    capabilities = capabilities,
    handlers = handlers,
})

lsp_config.phpactor.setup({
    cmd = { 'phpactor', 'language-server' },
    filetypes = { 'php' },
    handlers = handlers,
    capabilities = capabilities,
})

lsp_config.erlangls.setup({
    handlers = handlers,
    capabilities = capabilities,
})

lsp_config.clojure_lsp.setup({
    handlers = handlers,
    capabilities = capabilities,
})

lsp_config.html.setup({
    filetypes = { 'php', 'html', 'vue', 'twig' },
    handlers = {
        ["textDocument/hover"] = handlers["textDocument/hover"],
        ["textDocument/signaturehelp"] = handlers["textDocument/signaturehelp"],
        ["textDocument/rename"] = function() print("html renaming is disabled") end,
    },
    capabilities = capabilities,
    init_options = {
        embeddedLanguages = {
            css = true,
            javascript = true,
            php = true,
        }
    }
})

lsp_config.emmet_ls.setup({
    filetypes = { 'php', 'html', 'vue', 'twig' },
    handlers = {
        ["textDocument/hover"] = handlers["textDocument/hover"],
        ["textDocument/signaturehelp"] = handlers["textDocument/signaturehelp"],
        ["textDocument/rename"] = function() print("emmet_ls renaming is disabled") end,
    },
    capabilities = capabilities,
})

lsp_config.volar.setup({
    handlers = handlers,
    capabilities = capabilities,
})

lsp_config.tsserver.setup({
    handlers = handlers,
    capabilities = capabilities,
})

lsp_config.pyright.setup({
    handlers = handlers,
    capabilities = capabilities,
})

lsp_config.cmake.setup({
    handlers = handlers,
    capabilities = capabilities,
})

lsp_config.ocamllsp.setup({
    handlers = handlers,
    capabilities = capabilities,
})

lsp_config.swift_mesonls.setup({
    handlers = handlers,
    capabilities = capabilities,
})

lsp_config.cssls.setup({
    filetypes = { 'html', 'scss', 'css' },
    handlers = {
        ["textDocument/hover"] = handlers["textDocument/hover"],
        ["textDocument/signatureHelp"] = handlers["textDocument/signatureHelp"],
        ["textDocument/rename"] = function() print("cssls renaming is disabled") end,
    },
    capabilities = capabilities,
})

lsp_config.dartls.setup {
    cmd = { "dart", 'language-server', '--protocol=lsp' },
}

vim.api.nvim_command('au BufRead,BufNewFile *.xaml set filetype=xml')
vim.api.nvim_command('au BufRead,BufNewFile *.axaml set filetype=xml')
