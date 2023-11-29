-- vim options
vim.opt.shiftwidth = 2
vim.opt.tabstop = 2
vim.opt.relativenumber = true

-- general
lvim.log.level = "info"
lvim.format_on_save = {
  enabled = true,
  pattern = "*.lua",
  timeout = 1000,
}
-- to disable icons and use a minimalist setup, uncomment the following
-- lvim.use_icons = false

-- keymappings <https://www.lunarvim.org/docs/configuration/keybindings>
lvim.leader = ","
-- add your own keymapping

-- switch buffer-tabs left<->right with ctrl-l and ctrl-h
-- lvim.keys.normal_mode["<C-l>"] = ":BufferLineCycleNext<CR>"
-- lvim.keys.normal_mode["<C-h>"] = ":BufferLineCyclePrev<CR>"

-- jump to file
lvim.keys.normal_mode["<C-p>"] = ":Telescope find_files<CR>"

local utils = require "lvim.utils.modules"
local leap = require "leap"
local actions = utils.require_on_exported_call "lir.actions"
-- toggle file viewer
lvim.keys.normal_mode["<Leader>n"] = ":NvimTreeToggle<CR>"
lvim.keys.normal_mode["<Leader>e"] = ":NvimTreeFocus<CR>"
lvim.keys.normal_mode["x"] = false
lvim.keys.visual_mode["x"] = false
lvim.keys.normal_mode["S"] = "<Plug>(leap-backward)"
lvim.keys.normal_mode["W"] = "<Plug>(leap-from-window)"

local opts = { noremap = true, silent = true }

local function quickfix()
  vim.lsp.buf.code_action({
    filter = function(a) return a.isPreferred end,
    apply = true
  })
end


lvim.builtin.which_key.mappings["<C-p>"] = { "<cmd>Telescope find_files<CR>", "Find Files" }

-- Use which-key to add extra bindings with the leader-key prefix for things I don't use often but need

-- Change theme settings
lvim.colorscheme = "lunar"

lvim.builtin.alpha.active = true
lvim.builtin.alpha.mode = "dashboard"
lvim.builtin.terminal.active = true
lvim.builtin.nvimtree.setup.view.side = "left"
lvim.builtin.nvimtree.setup.renderer.icons.show.git = true
lvim.builtin.nvimtree.setup.renderer.icons.show.file = true
lvim.builtin.telescope.pickers.find_files.hidden = true

-- Automatically install missing parsers when entering buffer
lvim.builtin.treesitter.auto_install = true

-- lvim.builtin.treesitter.ignore_install = { "haskell" }

-- -- always installed on startup, useful for parsers without a strict filetype
lvim.builtin.treesitter.ensure_installed = { "comment", "markdown_inline", "regex" }

-- -- linters and formatters <https://www.lunarvim.org/docs/languages#lintingformatting>
local formatters = require "lvim.lsp.null-ls.formatters"
formatters.setup {
  {
    command = "prettier",
    filetypes = { "typescript", "typescriptreact" },
  },
}

local linters = require "lvim.lsp.null-ls.linters"
linters.setup {
  { command = "eslint", filetypes = { "typescriptreact", "typescript" } },
  {
    command = "shellcheck",
    args = { "--severity", "warning" },
  },
}

lvim.plugins = {
  {
    "pmizio/typescript-tools.nvim",
  },
  {
    "ggandor/leap.nvim",
    name = "leap",
    config = function()
      require('leap').add_default_mappings()
    end,
  },
  {
    'rmagatti/auto-session',
    config = function()
      require("auto-session").setup {
        log_level = "error",
        auto_session_suppress_dirs = { "~/", "~/Projects", "~/Downloads", "/" },
      }
    end
  }
}

-- Autocommands (`:help autocmd`) <https://neovim.io/doc/user/autocmd.html>
vim.api.nvim_create_autocmd("FileType", {
  pattern = "zsh",
  callback = function()
    -- let treesitter use bash highlight for zsh files as well
    require("nvim-treesitter.highlight").attach(0, "bash")
  end,
})

require('leap').add_default_mappings()
