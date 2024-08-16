local cmp = require 'cmp'

local symbols = {
    ["Function"]      = "󰊕 ",
    ["Method"]        = " ",
    ["Constructor"]   = " ",
    ["Reference"]     = " ",
    ["Class"]         = " ",
    ["Interface"]     = " ",
    ["Struct"]        = " ",
    ["Field"]         = " ",
    ["Property"]      = " ",
    ["Variable"]      = " ",
    ["Value"]         = " ",
    ["TypeParameter"] = " ",
    ["Unit"]          = " ",
    ["Constant"]      = " ",
    ["Enum"]          = " ",
    ["EnumMember"]    = " ",
    ["Keyword"]       = " ",
    ["Event"]         = " ",
    ["Snippet"]       = " ",
    ["Text"]          = "󰦨 ",
    ["File"]          = " ",
    ["Module"]        = " ",
    ["Folder"]        = " ",
}

---@param str string
---@return string
function trim_left(str)
    for i = 1, str:len(), 1 do
        if str[i] ~= " " then
            return string.sub(str, i, str:len())
        end
    end
    return str
end

---@param fn string
---@return string
local remove_params = function(fn)
    for i = 1, fn:len(), 1 do
        if string.sub(fn, i, i) == "(" then
            return string.sub(fn, 0, i - 1)
        end
    end
    return fn
end

---@param fn string
---@return string
local remove_template = function(fn)
    for i = 1, fn:len(), 1 do
        if string.sub(fn, i, i) == "<" then
            return string.sub(fn, 0, i - 1)
        end
    end
    return fn
end


cmp.setup({
    formatting = {
        expandable_indicator = false,
        fields = { 'abbr', 'menu', 'kind',  },
        format = function(_, vim_item)
            if symbols[vim_item.kind] ~= nil then
                vim_item.kind = symbols[vim_item.kind]
            end
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
    sources = cmp.config.sources(
        {
            { name = 'nvim_lsp' },
            { name = 'luasnip' },
        },
        {
            { name = 'buffer' },
        }
    )
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
