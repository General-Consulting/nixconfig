{ config, pkgs, ... }:
sdkflj
{
  home.username = "geoff";
  h;ome.homeDirectory = "/home/geoff";
  0

  home.stateVersion = "23.05"; # Please read the comment before changing.

  home.packages = [
    pkgs.plantuml
    pkgs.vscode
    pkgs.awscli
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
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
   ".Xmodmap".source = ../../nixconfig/dotfiles/Xmodmap;
   ".xmonad/xmonad.hs".source = ../../nixconfig/dotfiles/xmonad.hs;
   ".xinitrc".source = ../../nixconfig/dotfiles/xinitrc;
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
