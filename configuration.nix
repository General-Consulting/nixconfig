{ pkgs,  lib,  ... }:
{

  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (lib.getName pkg) [
      # Add additional package names here
      "vscode"
    ];
  imports = [ # Include the results of the hardware scan.
    #     ./home-manager
    <home-manager/nixos>
    ./hardware-configuration.nix
    ./nixos-hardware/common/cpu/amd
    ./nixos-hardware/common/gpu/amd
    ./nixos-hardware/common/cpu/amd/pstate.nix
  ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.kernelParams =
    [ "video=DP-1:3840x2160@60" "video=HDMI-A-1:3840x2160@60" ];

  networking.hostName = "nixos"; # Define your hostname.

  nix.settings.experimental-features = [ "nix-command" "flakes" ];
networking.interfaces = {
enp3s0.ipv4.addresses = [{
        address = "192.168.99.1";
        prefixLength = 32;
      }];
  };

  time.timeZone = "America/New_York";

  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  fonts.packages = with pkgs; [
    noto-fonts
    noto-fonts-emoji
    liberation_ttf
    fira-code
    fira-code-symbols
    mplus-outline-fonts.githubRelease
    dina-font
    proggyfonts
  ];

  services.xserver = {
    excludePackages = [ pkgs.xterm ];
    enable = true;
    videoDrivers = [ "displaylink" "modesetting" "amdgpu" ];
    dpi = 180;

    windowManager = {
      xmonad = {
        enable = true;
        enableContribAndExtras = false;
        extraPackages = hpkgs: with hpkgs; [ xmobar  xmonad-contrib xmonad-extras ];
        enableConfiguredRecompile = false;
        haskellPackages = pkgs.haskellPackages;
      };
    };

    xrandrHeads = [
      { output = "HDMI-1"; }
      {
        output = "DP-3";
        primary = true;
      }
    ];

    displayManager = {
      defaultSession = "none+xmonad";

      lightdm.enable = true;
      lightdm.greeters.enso = {
        enable = true;
        blur = true;
      };

      startx.enable = true;
    };

    xkb.layout = "us";
    # displayManager.sessionCommands = ''
    #   ${lib.getBin pkgs.xorg.xrandr}/bin/xrandr --setprovideroutputsource 1 0
    # '';
  };

  services.printing.enable = true;

  environment.pantheon.excludePackages = with pkgs.pantheon; [
    elementary-music
    elementary-photos
    elementary-videos
    epiphany
  ];

  hardware.pulseaudio = {
    enable = false;
    extraConfig = "load-module module-combine-sink";
    configFile = pkgs.runCommand "default.pa" {} ''
  sed 's/module-udev-detect$/module-udev-detect tsched=0/' \
    ${pkgs.pulseaudio}/etc/pulse/default.pa > $out
'';

  };
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
  };

  services.tailscale.enable = true;
  services.devmon.enable = true;
  services.gvfs.enable = true; 
  services.udisks2.enable = true;
  services.udev.packages = with pkgs; [gnome.gnome-settings-daemon];

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.geoff = {
    isNormalUser = true;
    description = "geoff";
    extraGroups = [ "networkmanager" "wheel" "docker" "root" "audio" "proton-vpn"];
    shell = pkgs.zsh;
    packages = with pkgs; [ home-manager ];
  };

  users.extraGroups.docker.members = [ "geoff" "minikube" ];

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.pulseaudio = true;

  environment.systemPackages = with pkgs; [
    protonvpn-gui
    networkmanagerapplet
    appeditor
    formatter
    cryptsetup
    umount
    gnome.simple-scan
    indicator-application-gtk3
    wget
    curl
    bind
    git
    vifm-full
    zsh
    oh-my-zsh
    alacritty
    gnumake
    fzf
    arandr
    dmenu
    nixpkgs-fmt
    trayer
    haskellPackages.xmonad
    haskellPackages.xmonad-contrib

    haskellPackages.xmobar
    neovim
    libclang
    cmake
    htop
    glibc
    libgcc
    gcc
    zig
    ripgrep
    stack
    nix-index
    helix
    feh
    moreutils
    linuxHeaders
    stalonetray
    spacenavd
    libspnav
    xorg.xmodmap
    fontfinder
    gnome.gnome-disk-utility
    gnomeExtensions.appindicator
  ];

  environment.variables = {
    GDK_SCALE = "2";
    GDK_DPI_SCALE = "0.5";
  };

  programs.neovim = {
    viAlias = true;
    vimAlias = true;
  };

  programs.dconf.enable = true;
  programs.noisetorch.enable = true;

  hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = true;

  hardware.spacenavd.enable = true;

  #  programs.ssh.extraConfig = ''
  #    IdentityFile /home/geoff/.ssh/id_rsa_github
  #  '';

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    ohMyZsh.theme = "lambda";
    ohMyZsh.enable = true;
    ohMyZsh.plugins = [ "git" ];
    autosuggestions.enable = true;
    syntaxHighlighting.enable = true;
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.avahi = {
    enable = false;
    publish.enable = true;
  };

  services.blueman.enable = true;



# services = {
#    cloudflared = {
#      enable = true;
#      tunnels = {
#        "eedcc6cc-bdc2-44ba-a4d8-23f5d043b2a2" = {
#          credentialsFile = "/home/geoff/.cloudflared/eedcc6cc-bdc2-44ba-a4d8-23f5d043b2a2.json";
#          ingress = {
#            "tmp1.vteng.io" = { service = "http://localhost:8001"; };
#          };
#          default = "http_status:404";
#        };
#      };
#    };
#  };

  # Open ports in the firewall.
  # Or disable the firewall altogether.
  networking.networkmanager.enable = true;


  systemd.services.NetworkManager-wait-online.enable = true;

  system.stateVersion = "24.05";

  networking.firewall = {
      enable = true;
      allowedTCPPorts = [22];
      logRefusedPackets = true;
  };

  virtualisation = { 
    docker = { 
      enable = true; 
    }; 
  };


  networking.extraHosts = ''
    	'';

}

