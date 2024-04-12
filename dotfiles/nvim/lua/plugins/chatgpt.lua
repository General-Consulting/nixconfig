{
  "jackMort/ChatGPT.nvim",
    event = "VeryLazy",
    config = function()
      require("chatgpt").setup()
    end,
    dependencies = {
      "MunifTanjim/nui.nvim",
      "nvim-lua/plenary.nvim",
      "folke/trouble.nvim",
      "nvim-telescope/telescope.nvim"
    }
  keys = {
    {"<leader>cc" , "<cmd>ChatGPT<CR>", desc = "ChatGPT" },
    {"<Leader>ce" , "<cmd>ChatGPTEditWithInstruction<CR>", "Edit with instruction", mode = { "n", "v" } },
    {"<Leader>cg" , "<cmd>ChatGPTRun grammar_correction<CR>", "Grammar Correction", mode = { "n", "v" } },
    {"<Leader>ct" , "<cmd>ChatGPTRun translate<CR>", "Translate", mode = { "n", "v" } },
    {"<Leader>ck" , "<cmd>ChatGPTRun keywords<CR>", "Keywords", mode = { "n", "v" } },
    {"<Leader>cd" , "<cmd>ChatGPTRun docstring<CR>", "Docstring", mode = { "n", "v" } },
    {"<Leader>ca" , "<cmd>ChatGPTRun add_tests<CR>", "Add Tests", mode = { "n", "v" } },
    {"<Leader>co" , "<cmd>ChatGPTRun optimize_code<CR>", "Optimize Code", mode = { "n", "v" } },
    {"<Leader>cs" , "<cmd>ChatGPTRun summarize<CR>", "Summarize", mode = { "n", "v" } },
    {"<Leader>cf" , "<cmd>ChatGPTRun fix_bugs<CR>", "Fix Bugs", mode = { "n", "v" } },
    {"<Leader>cx" , "<cmd>ChatGPTRun explain_code<CR>", "Explain Code", mode = { "n", "v" } },
    {"<Leader>cr" , "<cmd>ChatGPTRun roxygen_edit<CR>", "Roxygen Edit", mode = { "n", "v" } },
    {"<Leader>cl" , "<cmd>ChatGPTRun code_readability_analysis<CR>", "Code Readability Analysis", mode = { "n", "v" } },
}
  }
