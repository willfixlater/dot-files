{ pkgs, ... }:

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

    # zsh = {
    #   enable = true;
    #   oh-my-zsh = {
    #     enable = true;
    #     theme = "spaceship";
    #     plugins = [ "git" ];
    #   };
    #   plugins = [
    #     {
    #       name = "spaceship";
    #       file = "spaceship.zsh";
    #       src = pkgs.fetchFromGitHub {
    #         owner = "denysdovhan";
    #         repo = "spaceship-prompt";
    #         rev = "v3.8.0";
    #         sha256 = "1945kz9wrgna9pz5cw4c6s8dbhr1inmwas2ny5dksnj1jnd1kcks";
    #       };
    #     }
    #   ];
    # };
  };

  home = {
    sessionVariables = {
      EDITOR = "emacs -nw";
    };
  };
}
