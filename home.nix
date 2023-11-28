{ pkgs, ... }:
{

  home.username = "geoff";
  home.homeDirectory = "/home/geoff";

  home.stateVersion = "23.11"; 

  home.packages = [
    pkgs.plantuml
    pkgs.vscode
    pkgs.awscli2
    pkgs.obsidian
    pkgs.tmux
    pkgs.tailscale
    pkgs.kubectl
    pkgs.synergy
    pkgs.direnv
    pkgs.chromium
    pkgs.google-chrome
    pkgs.zoom-us
    pkgs.xclip
    pkgs.scrot
    pkgs.newman
    pkgs.xorg.xev
    pkgs.bruno
    pkgs.nerdfonts
    pkgs.shellcheck
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
   ".Xmodmap".source = ./dotfiles/Xmodmap;
   ".xmonad/xmonad.hs".source = ./dotfiles/xmonad.hs;
   ".xinitrc".source = ./dotfiles/xinitrc;
  };

  programs.rofi = {
    enable = true;
    terminal = "${pkgs.alacritty}/bin/alacritty";
  };

  programs.zsh = {
    enable = true;
    initExtra = ''
      set -o vi
    '';
  };


  # if you don't want to manage your shell through Home Manager.
  home.sessionVariables = {
    EDITOR = "vim";
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
