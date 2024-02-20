{ pkgs, lib, ... }:
let
  obsidian = lib.throwIf (lib.versionOlder "1.5.3" pkgs.obsidian.version)
    "Obsidian no longer requires EOL Electron" (pkgs.obsidian.override {
      electron = pkgs.electron_25.overrideAttrs (_: {
        preFixup =
          "patchelf --add-needed ${pkgs.libglvnd}/lib/libEGL.so.1 $out/bin/electron"; # NixOS/nixpkgs#272912
        meta.knownVulnerabilities = [ ]; # NixOS/nixpkgs#273611
      });
    });
in {
  home.username = "geoff";
  home.homeDirectory = "/home/geoff";

  home.stateVersion = "23.11";
  extraGroups = [ "networkmanager" "wheel" "docker" "root" "podman" ];

  home.packages = with pkgs; [
    plantuml
    vscode
    awscli2
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
    obsidian
    ndi
    busybox
    podman-compose
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

  programs.rofi = {
    enable = true;
    terminal = "${pkgs.alacritty}/bin/alacritty";
  };

  programs.zsh = {
    enable = true;
    initExtra = "";
    shellAliases = {
      ll = "ls -l";
      docker-compose = "podman-compose";
    };
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
      merge = { tool = "vimdiff"; };
      diff = { tool = "vimdiff"; };
    };
  };

  programs.vim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [ nerdtree ];
  };

}
