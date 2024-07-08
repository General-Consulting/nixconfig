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
              (vscode-with-extensions.override {
                vscode = vscode;
                vscodeExtensions = with vscode-extensions; [
                  jnoortheen.nix-ide
                  vscodevim.vim
                  yzhang.markdown-all-in-one
                  eamodio.gitlens
                  editorconfig.editorconfig
                  christian-kohler.npm-intellisense
                  bradlc.vscode-tailwindcss
                  wix.vscode-import-cost
                ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [


                  {
                    name = "autoclosetabs";
                    publisher = "Zwyx";
                    version = "1.1.2";
                    sha256 = "sha256-yZUAyBnGYFZQ7VXYsTfzF2rl6rAQ+DeZVC2FNMzw/uc=";
                  }
                  {
                    name = "deepdark-material";
                    publisher = "Nimda";
                    version = "3.3.1";
                    sha256 = "sha256-gc7B3h+r4UXO0WSVsscOa5nY4RRxG5XX3zrC1E1WJ3k=";
                  }
                  {
                    name = "copilot";
                    publisher = "Github";
                    version = "1.165.0";
                    sha256 = "sha256-8HvWb5zaoUdZ+BsAnW2TM20LqGwZshxgeJDEYKZOFgg=";
                  }
                  {
                    name = "back-n-forth";
                    publisher = "nick-rudenko";
                    version = "3.1.1";
                    sha256 = "sha256-yircrP2CjlTWd0thVYoOip/KPve24Ivr9f6HbJN0Haw=";
                  }
                  {
                    name = "smart-tabs";
                    publisher = "Valsorym";
                    version = "1.3.2";
                    sha256 = "sha256-RRL9DHQnZT64wIBvKC+f+6ga4kRptH98zpljfHc+Cu4=";
                  }
                  {
                    name = "prettier-vscode";
                    publisher = "esbenp";
                    version = "10.4.0";
                    sha256 = "sha256-8+90cZpqyH+wBgPFaX5GaU6E02yBWUoB+T9C2z2Ix8c=";
                  }
                ];
              }
            )
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
    haskellPackages.xmonad
    haskellPackages.xmobar
    fzf
    lazygit
    feh
    git
    git-lfs
    picom
    stalonetray
    xorg.xmodmap
    freecad
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


  programs.neovim = {
      enable = true;
      extraLuaPackages = ps: [ ps.magick ];
    };

  xsession.profileExtra = ''

  '';

}
