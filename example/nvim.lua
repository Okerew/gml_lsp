-- GML LSP Configuration for Neovim
local lspconfig = require('lspconfig')
local configs = require('lspconfig.configs')

if not configs.gml_lsp then
  configs.gml_lsp = {
    default_config = {
      -- The command to start your GML language server.
      -- Make sure 'gml_lsp' is in your system's PATH.
      cmd = { 'gml_lsp' },
      -- Filetypes this LSP should attach to.
      filetypes = { 'gml' },
      root_dir = lspconfig.util.root_pattern('.git', '*.yyp'),
      settings = {},
      -- Gml lsp server doesn't use init_options, so this can be empty.
      init_options = {}
    },
    docs = {
      description = [[
        GML Language Server for GameMaker Studio 2 development.
        Provides completion, hover, and semantic highlighting.
      ]]
    }
  }
end

lspconfig.gml_lsp.setup({
  on_attach = function(client, bufnr)
    -- Enable completion triggered by <c-x><c-o>
    vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')
    
    -- Enable semantic tokens, which your server provides
    if client.server_capabilities.semanticTokensProvider then
      vim.lsp.semantic_tokens.start(bufnr, client.id)
    end
    
    -- Mappings for features SUPPORTED by gml_lsp
    local opts = { noremap=true, silent=true, buffer=bufnr }
    
    -- Show information about the symbol under the cursor (Hover)
    vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
    
  end,
  
  flags = {
    debounce_text_changes = 150,
  },
  
  -- Client capabilities, especially for semantic tokens
  capabilities = vim.tbl_deep_extend('force',  
    require('cmp_nvim_lsp').default_capabilities(),
    {
      textDocument = {
        semanticTokens = {
          requests = {
            range = false,
            full = {
              delta = false
            }
          },
          -- These types and modifiers match what your Go server provides.
          tokenTypes = {
            'namespace', 'type', 'class', 'enum', 'interface', 'struct',
            'typeParameter', 'parameter', 'variable', 'property', 'enumMember',
            'event', 'function', 'method', 'macro', 'keyword', 'modifier',
            'comment', 'string', 'number', 'regexp', 'operator'
          },
          tokenModifiers = {
            'declaration', 'definition', 'readonly', 'static', 'deprecated',
            'abstract', 'async', 'modification', 'documentation', 'defaultLibrary'
          }
        }
      }
    }
  )
})

vim.api.nvim_create_autocmd({"BufRead", "BufNewFile"}, {
  pattern = {"*.gml"},
  callback = function()
    vim.bo.filetype = "gml"
    vim.bo.commentstring = "// %s"
    vim.bo.expandtab = true
    vim.bo.shiftwidth = 4 -- Common for GML
    vim.bo.tabstop = 4
    vim.bo.softtabstop = 4
  end,
})

vim.api.nvim_create_autocmd("FileType", {
  pattern = "gml",
  callback = function(args)
    -- Define GML-specific highlight groups that work with your LSP's semantic tokens
    local highlights = {
      ["@lsp.type.keyword"] = { link = "Keyword" },
      ["@lsp.type.function"] = { link = "Function" },
      ["@lsp.type.method"] = { link = "Function" },
      ["@lsp.type.variable"] = { link = "Identifier" },
      ["@lsp.type.property"] = { fg = "#d08770" },
      ["@lsp.type.parameter"] = { link = "Identifier" },
      ["@lsp.type.string"] = { link = "String" },
      ["@lsp.type.number"] = { link = "Number" },
      ["@lsp.type.comment"] = { link = "Comment" },
      ["@lsp.type.operator"] = { link = "Operator" },
      ["@lsp.type.macro"] = { link = "Macro" },
    }
    
    for group, opts in pairs(highlights) do
      vim.api.nvim_set_hl(0, group, opts)
    end
  end,
})

-- Setup nvim-cmp for autocompletion
local cmp_status, cmp = pcall(require, 'cmp')
if cmp_status then
  cmp.setup({
    sources = cmp.config.sources({
      { name = 'nvim_lsp', priority = 1000 },
      { name = 'buffer', priority = 500 },
      { name = 'path', priority = 250 }
    }),
    experimental = {
      ghost_text = true,
    }
  })
end
