{ pkgs, ... }:
#let
#  obsidian = lib.throwIf (lib.versionOlder "1.5.3" pkgs.obsidian.version)
#    "Obsidian no longer requires EOL Electron" (pkgs.obsidian.override {
#      electron = pkgs.electron_25.overrideAttrs (_: {
#        preFixup =
#          "patchelf --add-needed ${pkgs.libglvnd}/lib/libEGL.so.1 $out/bin/electron"; # NixOS/nixpkgs#272912
#        meta.knownVulnerabilities = [ ]; # NixOS/nixpkgs#273611
#      });
#    });
#in 
{
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url =
        "https://github.com/nix-community/neovim-nightly-overlay/archive/master.tar.gz";
    }))
  ];
  home.username = "geoff";
  home.homeDirectory = "/home/geoff";

  home.stateVersion = "24.11";

  home.packages = with pkgs; [
    plantuml
    awscli2
    tmux
    tailscale
    kubectl
    synergy
    direnv
    google-chrome
    flameshot
    xclip
    scrot
    newman
    xorg.xev
    nerdfonts
    shellcheck
    trayer
    fzf
    lazygit
    feh
    git
    git-lfs
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
    openscad-unstable
    ffmpeg
    gnome.nautilus
    cargo
    slack
    slack-cli
    jamesdsp
    nix-du
    graphviz
    protonvpn-gui
    protonvpn-cli
    discord-ptb
    obsidian
    ndi
    busybox
    zoom-us
    kitty
    handbrake
    imagemagick
    stylua
      (luajit.withPackages (p: with p; [ luacheck magick ]))
      lua-language-server
    lua54Packages.luarocks-nix
    luarocks-packages-updater
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

  services.blueman-applet  = {
      enable = true;
    };

  programs.rofi = {
    enable = true;
    terminal = "${pkgs.alacritty}/bin/alacritty";
  };

  programs.zsh = {
    enable = true;
    initExtra = "";
    shellAliases = { ll = "ls -l"; };
  };

  programs.obs-studio = {
    enable = true;
    plugins = with pkgs.obs-studio-plugins; [ obs-ndi ];
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
      merge = { tool = "lazygit"; };
      diff = { tool = "lazygit"; };
      editor = "vim";
    };
  };

  programs.vim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [ nerdtree ];
  };

  dconf.settings = {
      "org/gnome/nautilus/preferences" = {
          default-folder-viewer = "list-view";
          default-sort-order = "type";
        };
    };


  programs.neovim = {
      enable = true;
      extraLuaPackages = ps: [ ps.magick ];
    };

  xsession.profileExtra = ''

  '';

}
