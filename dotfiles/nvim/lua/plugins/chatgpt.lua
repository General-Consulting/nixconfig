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
    {"<Leader>ac" , "<cmd>ChatGPT<CR>", "ChatGPT" },
    {"<Leader>ae" , "<cmd>ChatGPTEditWithInstruction<CR>", "Edit with instruction", mode = { "n", "v" } },
    {"<Leader>ag" , "<cmd>ChatGPTRun grammar_correction<CR>", "Grammar Correction", mode = { "n", "v" } },
    {"<Leader>at" , "<cmd>ChatGPTRun translate<CR>", "Translate", mode = { "n", "v" } },
    {"<Leader>ak" , "<cmd>ChatGPTRun keywords<CR>", "Keywords", mode = { "n", "v" } },
    {"<Leader>ad" , "<cmd>ChatGPTRun docstring<CR>", "Docstring", mode = { "n", "v" } },
    {"<Leader>aa" , "<cmd>ChatGPTRun add_tests<CR>", "Add Tests", mode = { "n", "v" } },
    {"<Leader>ao" , "<cmd>ChatGPTRun optimize_code<CR>", "Optimize Code", mode = { "n", "v" } },
    {"<Leader>as" , "<cmd>ChatGPTRun summarize<CR>", "Summarize", mode = { "n", "v" } },
    {"<Leader>af" , "<cmd>ChatGPTRun fix_bugs<CR>", "Fix Bugs", mode = { "n", "v" } },
    {"<Leader>ax" , "<cmd>ChatGPTRun explain_code<CR>", "Explain Code", mode = { "n", "v" } },
    {"<Leader>ar" , "<cmd>ChatGPTRun roxygen_edit<CR>", "Roxygen Edit", mode = { "n", "v" } },
    {"<Leader>al" , "<cmd>ChatGPTRun code_readability_analysis<CR>", "Code Readability Analysis", mode = { "n", "v" } },
}
  }
