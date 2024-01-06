local cmp = require 'cmp'

local icons = {
    ["Function"]  = "󰊕",
    ["Class"]     = "",
    ["Interface"] = "",
    ["Struct"]    = "",
    ["Field"]     = "",
    ["Variable"]  = "",
    ["Enum"]      = "",
    ["EnumMember"]= "",
    ["Keyword"]   = "",
    ["Snippet"]   = "",
    ["Text"]      = "󰦨",
}

cmp.setup({
    formatting = {
       expandable_indicator = true,
       fields = { 'abbr','kind', },
       format = function (entry, vim_item)

            if vim_item.kind == "Function" then
                vim_item.abbr = vim_item.word
            end

            vim_item.kind = icons[vim_item.kind]
            return vim_item;
       end,

    },
    snippet = {
        expand = function(args)
            require('luasnip').lsp_expand(args.body)
        end,
    },
    window = {
        documentation = cmp.config.window.bordered(),
        completion = cmp.config.window.bordered(),
        scrollbar = false,
    },
    mapping = cmp.mapping.preset.insert({
        ['<C-b>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs(4),
        ['<C-Space>'] = cmp.mapping.complete(),
        ['<C-e>'] = cmp.mapping.abort(),
        ['<TAB>'] = cmp.mapping.confirm({ select = true }),
    }),
    sources = cmp.config.sources({
            { name = 'nvim_lsp' },
            { name = 'luasnip' },
        },
        {
            { name = 'buffer' },
        })
})

cmp.setup.filetype('gitcommit', {
    sources = cmp.config.sources({
        { name = 'git' },
    }, {
        { name = 'buffer' },
    })
})

cmp.setup.cmdline({ '/', '?' }, {
    mapping = cmp.mapping.preset.cmdline(),
    sources = {
        { name = 'buffer' }
    }
})

cmp.setup.cmdline(':', {
    mapping = cmp.mapping.preset.cmdline(),
    sources = cmp.config.sources({
        { name = 'path' }
    }, {
        { name = 'cmdline' }
    })
})

