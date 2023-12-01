{ config, pkgs, lib, ... }:

{

  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (lib.getName pkg) [
      # Add additional package names here
      "vscode"
    ];
  imports =
    [ # Include the results of the hardware scan.
     <home-manager/nixos>
     ./hardware-configuration.nix
     ./nixos-hardware/common/cpu/amd
     ./nixos-hardware/common/gpu/amd
     ./nixos-hardware/common/cpu/amd/pstate.nix
    ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.kernelParams = [
  "video=DP-3:3840x2160@60"
  "video=HDMI-A-1:3840x2160@60"
  ];

  networking.hostName = "nixos"; # Define your hostname.

  nix.settings.experimental-features = ["nix-command" "flakes"];

  networking.networkmanager.enable = true;

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

  i18n.consoleFont = "latarcyrheb-sun32";

  fonts.packages = with pkgs; [
  noto-fonts
  noto-fonts-cjk
  noto-fonts-emoji
  liberation_ttf
  fira-code
  fira-code-symbols
  mplus-outline-fonts.githubRelease
  dina-font
  proggyfonts
];

  services.xserver = {
    excludePackages = [pkgs.xterm];
    enable = true;
    videoDrivers = [ "displaylink" "modesetting" "amdgpu"];
    dpi = 180;

    windowManager = {
      xmonad = {
        enable = true;
        enableContribAndExtras = true;
        extraPackages = hpkgs: [
          hpkgs.xmobar
        ];
      };
    };

    xrandrHeads = [
     {
       output = "HDMI-1";
       primary = true;
     }
     {
       output = "DP-3";
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

    layout = "us";
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



  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    #jack.enable = true;
  };

  services.tailscale.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.geoff = {
    isNormalUser = true;
    description = "geoff";
    extraGroups = [ "networkmanager" "wheel" "docker"];
    packages = with pkgs; [
      home-manager
      xorg.xmodmap
    ];
    shell = pkgs.zsh;
  };

  nixpkgs.config.allowUnfree = true;

  home-manager.extraSpecialArgs = {
    inherit pkgs;
    inherit lib;
  };

  # $ nix search wget
  environment.systemPackages = with pkgs; [
    appeditor
    formatter
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
    lunarvim
    trayer
    haskellPackages.xmonad
    haskellPackages.xmobar
    neovim
    libclang
    cmake
    gcc
    zig
    ripgrep
  ];


  environment.variables = {
    GDK_SCALE = "2";
    GDK_DPI_SCALE = "0.5";
  };

  programs.neovim = {
  viAlias = true;
  vimAlias = true;
};

  programs.noisetorch.enable = true;
 
  hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = true;

  programs.ssh.extraConfig = ''
    IdentityFile /home/geoff/.ssh/id_rsa_github
  '';
 
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

  # Open ports in the firewall.
  # Or disable the firewall altogether.
  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [ 2375 22 80 443 ];
  networking.firewall.allowedTCPPortRanges = [ {from = 4000; to = 5550;} ];
  networking.firewall.allowedUDPPorts = [ 24800 ];

  system.stateVersion = "24.05"; 

  virtualisation.docker.enable = true;
  
  networking.extraHosts = 
	'' 
		192.168.0.107 nixos
	'';

}

