return {
    'Vonr/align.nvim',
    branch = "v2",
    lazy = true,
    init = function()
    end
    keys = { 
      { 
        "<leader>aa", 
        function()
          require('align').align_to_char({
            length =1,
          }), 
        desc = "align to char" 
      } 
    },
}
