{ config, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.09"; # Did you read the comment?

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Define your hostname.
  networking.hostName = "nixos";

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Fira Code";
    consoleKeyMap = "us";
    defaultLocale = "en_AU.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Australia/Sydney";

  # Disable firewall
  networking.firewall.enable = false;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  environment = {
    systemPackages = with pkgs; [
      (import /etc/nixos/emacs.nix { inherit pkgs; })
      git
      dmenu
      rxvt_unicode
    ];
  };

  services = {
    openssh.enable = true;
    printing.enable = true;

    xserver = {
      enable = true;
      layout = "us";
      libinput.enable = true;

      desktopManager = {
        gnome3.enable = true;
      };
    };
  };

  fonts.fonts = [
    pkgs.fira-code
  ];

  users.users = {
    shdzzl = {
      isNormalUser = true;
      home = "/home/shdzzl";
      description = "Shayden Martin";
      extraGroups = [ "wheel" ];
    };
  };
}
