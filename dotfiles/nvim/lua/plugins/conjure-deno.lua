return {
  'sigmaSd/conjure-deno',
  depenpendcies = {{
    'nvim-treesitter/nvim-treesitter',
    config = function()
      require('nvim-treesitter.install').update({ with_sync = true })
    end,
  },
    {
      'Olical/conjure'
    }}
}
