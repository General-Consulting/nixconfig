{ config, pkgs, ... }:

{
  home.username = "geoff";
  home.homeDirectory = "/home/geoff";

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
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
   ".Xmodmap".source = ../../nixconfig/dotfiles/Xmodmap;
  };

  programs.rofi = {
    enable = true;
    terminal = "${pkgs.alacritty}/bin/alacritty";
  };


  programs.bash = {
    enable = true;
    historyControl = [ "ignoredups" ];
    historySize = 1000000;
    historyFileSize = 1000000;
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
