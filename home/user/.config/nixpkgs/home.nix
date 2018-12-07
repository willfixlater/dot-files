{ pkgs, ... }:

let
aliases = { em = "emacs -nw"; };
in
{
  nixpkgs.config.allowUnfree = true;

  xsession = {
    enable = true;
    windowManager = {
      xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
    };
    pointerCursor = {
      name = "Vanilla-DMZ";
      package = pkgs.vanilla-dmz;
    };
  };

  xresources.properties = import ./xresources.nix { inherit pkgs; };

  services = {
    compton = import ./compton.nix { inherit pkgs; };
    polybar = import ./polybar.nix { inherit pkgs; };
  };  

  programs = {
    home-manager = {
      enable = true;
      path = "https://github.com/rycee/home-manager/archive/master.tar.gz";
    };

    bash = {
      enable = true;
      shellAliases = aliases;
    };


    zsh = {
      enable = true;
      shellAliases = aliases;
      oh-my-zsh = {
        enable = true;
        theme = "nanotech";
        plugins = [ "git" ];
      };
    };

    urxvt = import ./urxvt.nix { inherit pkgs; };
    rofi = import ./rofi.nix { inherit pkgs; };

    feh = {
      enable = true;
    };

    git = {
      enable = true;
      userName = "shdzzl";
      userEmail = "mail@shayden.me";
    };
  };

  home = {
    sessionVariables = {
      EDITOR = "emacs -nw";
    };
  };
}
