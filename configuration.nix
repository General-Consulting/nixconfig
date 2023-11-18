
{ config, pkgs, lib, ... }:

{

  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (lib.getName pkg) [
      # Add additional package names here
      "vscode"
    ];
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
     # ./home-manager.nix
     /home/geoff/nixconfig/nixos-hardware/common/cpu/amd
     /home/geoff/nixconfig/nixos-hardware/common/gpu/amd
     /home/geoff/nixconfig/nixos-hardware/common/cpu/amd/pstate.nix
    ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

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

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    videoDrivers = [ "displaylink" "modesetting" "amdgpu"];
    desktopManager = {
      plasma5.enable = true;
    };
    displayManager.sddm.enable = true;
    layout = "us";
    xkbVariant = "";
    displayManager.setupCommands = ''
      LEFT='DP-1'
      RIGHT='HDMI-1'
      ${pkgs.xorg.xrandr}/bin/xrandr --output $LEFT --left-of $RIGHT
    '';
    # displayManager.sessionCommands = ''
    #   ${lib.getBin pkgs.xorg.xrandr}/bin/xrandr --setprovideroutputsource 1 0
    # '';
  };

  services.printing.enable = true;


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
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    wget
    curl
    bind
    git
  ];


services.xserver.displayManager.sessionCommands =
  let
    xmodmapConfig = pkgs.writeText "xkb-layout" ''
   ! Clear the current caps lock definition
   clear Lock
  
   ! Make Caps Lock act as a Mode_switch key
   keycode 66 = Mode_switch

   ! fix the 
   keycode 56 = b B

   ! caps + esc = backtick, shift + esc = tilde
   keysym Escape = Escape asciitilde grave 

   ! caps + hkjl = arrow keys
   keysym h = h H Left NoSymbol Left
   keysym j = j J Down NoSymbol Down
   keysym k = k K Up NoSymbol Up
   keysym l = l L Right NoSymbol Right
    '';
  in
  "${pkgs.xorg.xmodmap}/bin/xmodmap ${xmodmapConfig}";



  programs.noisetorch.enable = true;


 
  hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = true;
 
 
 
 
  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # Or disable the firewall altogether.
  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [ 2375 22 80 443 4000];
  networking.firewall.allowedUDPPorts = [ 24800 ];

  system.stateVersion = "23.05"; 

  virtualisation.docker.enable = true;
  
  networking.extraHosts = 
	'' 
		192.168.0.107 nixos
	'';

}

