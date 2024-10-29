local treesitter_postfix = require("luasnip.extras.treesitter_postfix").treesitter_postfix
local luasnip = require("luasnip")
local s = luasnip.snippet
local i = luasnip.insert_node
local t = luasnip.text_node

return {
    treesitter_postfix({
        trig = "ts",
        matchTSNode = {
            query = "(comment) @foo",
            query_lang = "typescript"
        },
    }, {
        t "// @ts-expect-error",
    }),

    s("test", {
        i(1), t "text", i(2), t "text again", i(3)
    })
}
