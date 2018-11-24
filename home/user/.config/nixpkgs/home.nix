{ pkgs, ... }:

{
  xsession = {
    enable = true;
    windowManager = {
      xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
    };
  };

  home = {
    sessionVariables = {
      EDITOR = "emacs -nw";
    };
  };

  services = {
    compton = {
      enable = true;
      shadow = false;
      noDockShadow = true;
      shadowOffsets = [ (-6) (-6) ];
    };

    polybar = import ./polybar.nix { inherit pkgs; };
  };  

  programs = {
    home-manager = {
      enable = true;
      path = "https://github.com/rycee/home-manager/archive/master.tar.gz";
    };

    rofi = {
      enable = true;
    };

    feh = {
      enable = true;
    };

    git = {
      enable = true;
      userName = "shdzzl";
      userEmail = "mail@shayden.me";
    };
  };
}
