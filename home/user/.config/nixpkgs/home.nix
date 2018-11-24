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

    polybar = {
      enable = true;
      script = "polybar main &";
      extraConfig = ''
        [colors]
        bar-background = #00000000
        bar-foreground = #ffffff
      '';
      config = {
        "bar/main" = {
          width = "100%";
          height = 46;
          bottom = true;
          fixed-center = true;
          background = "\${colors.bar-background}";
          foreground = "\${colors.bar-foreground}";
          line-size = 3;
          line-color = "#ff0000";
          padding-left = 3;
          padding-right = 3;
          module-margin-left = 2;
          module-margin-right = 2;
          modules-right = "filesystem date";
          cursor-click = "pointer";
          cursor-scroll = "ns-resize";
        };

        "module/filesystem" = {
          type = "internal/fs";
          interval = 30;
          mount-0 = "/";
          label-mounted = "FS: %percentage_used%%";
        };
        "module/date" = {
          type = "internal/date";
          interval = 5;
          date = "%B %d · %H:%M";
          date-alt = " %Y-%m-%d";
          label = "%date%";
        };
      };
    };
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
