{ pkgs, ... }:

let
  aliases = {
    sudo = "sudo ";
    em = "emacs -nw";
    copy-nix = "cp-system-nixos-files";
    switch-nix = "nixos-rebuild switch";
    switch-home = "home-manager switch";
    listsure-prod-db-tunnel = "ssh -Av -L9999:localhost:9999 listsure-prod-bastion -t ssh -v -L9999:la1mf6w11oez2v9.cyvjn25ipdu3.ap-southeast-2.rds.amazonaws.com:5432 -N localhost";
    listsure-test-db-tunnel = "ssh -Av -L9999:localhost:9999 listsure-test-bastion -t ssh -v -L9999:la1c9ef2rahe90v.cyvjn25ipdu3.ap-southeast-2.rds.amazonaws.com:5432 -N localhost";
    listsure-connect-to-db = "psql -h localhost -p 9999 listsure listsuredba";
  };
in
{
  nixpkgs.config.allowUnfree = true;

  home = {
    sessionVariables = {
      EDITOR = "emacs -nw";
      _JAVA_AWT_WM_NONREPARENTING = 1;
    };

    file = {
      bin.source = ./sources/bin;
      ".lein".source = ./sources/lein;
      ".fehbg".source = ./sources/feh/fehbg;
      ".fehbg_image".source = ./sources/feh/fehbg_image;
    };
  };

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
}
