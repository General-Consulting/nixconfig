{ pkgs, ... }:
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
    xclip
    scrot
    newman
    xorg.xev
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
    xorg.xmodmap
    vimb
    pavucontrol
    shotcut
    gnucash
    mplayer
    firefox
    gopls
    pyright
    openscad
    ffmpeg
    gnome.nautilus
    cargo
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

   ".vimrc".source = ./dotfiles/vimrc;

   ".config/helix/" = {
      source = ./dotfiles/helix;
      recursive = true;
    };
   "monitors.sh".source = ./dotfiles/monitors.sh;
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
  };

}
