
home-manager.users.geoff = { pkgs, ... }:
{

  home.username = "geoff";
  home.homeDirectory = "/home/geoff";

  home.stateVersion = "23.11"; 

  home.packages = with pkgs; [
    plantuml
    vscode
    awscli2
    obsidian
    tmux
    tailscale
    kubectl
    synergy
    direnv
    chromium
    google-chrome
    zoom-us
    xclip
    scrot
    newman
    xorg.xev
    bruno
    nerdfonts
    shellcheck
    trayer
    haskellPackages.xmonad
    haskellPackages.xmobar
    neovim
    fzf
    lazygit
    feh
    picom
    stalonetray
  ];

  home.file = {
   ".Xmodmap".source = ./dotfiles/Xmodmap;
   ".xmonad/" = {
      source = ./dotfiles/.xmonad;
      recursive = true;
    };
   ".config/nvim/" = {
      source = ./dotfiles/nvim;
      recursive = true;
    };
   ".config/helix/" = {
      source = ./dotfiles/helix;
      recursive = true;
    };
  };

  programs.rofi = {
    enable = true;
    terminal = "${pkgs.alacritty}/bin/alacritty";
  };

  programs.zsh = {
    enable = true;
    initExtra = ''
    '';
      shellAliases = {
    ll = "ls -l";
  };
  };


  # if you don't want to manage your shell through Home Manager.
  home.sessionVariables = {
    EDITOR = "lvim";
  };


  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
    nix-direnv.enable = true;
  };

  programs.git = {
    delta.enable = true;
    userName = "Geoff Golder";
    userEmail = "geoff@generalconsulting.io";

   extraConfig = {
    merge = { tool = "vimdiff"; };
    diff  = { tool = "vimdiff"; };
   };
  };

  programs.vim = {
   enable = true;
   plugins = with pkgs.vimPlugins; [nerdtree];
   extraConfig = builtins.readFile ../../nixconfig/dotfiles/vimrc;
  };

}
