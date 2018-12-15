{ pkgs, ... }:

{
  enable = true;
  # NOTE: When starting polybar from the systemd user service, it uses the
  # default cursor theme and not the one selected in home.nix. The no-op script
  # below is a hacky work-around and I have handed the starting of polybar off
  # to xmonad.
  script = "";
  extraConfig = ''
    [colors]
      bar-background = #00000000
      bar-foreground = #ffffff
      backlight-active = #ffffff
      backlight-inactive = #ffffff
      volume-active = #ffffff
      volume-inactive = #ffffff
    [fonts]
      fira-code = Fira Code:pixelsize=14;3
      symbola = Symbola:pixelsize=14;3
  '';
  config = {
    "bar/main" = {
      width = "100%";
      height = 46;
      bottom = true;
      fixed-center = true;
      font-0 = "\${fonts.fira-code}";
      font-1 = "\${fonts.symbola}";
      background = "\${colors.bar-background}";
      foreground = "\${colors.bar-foreground}";
      line-size = 3;
      padding-left = 3;
      padding-right = 3;
      module-margin-left = 1;
      module-margin-right = 1;
      modules-left = "backlight volume";
      modules-right = "cpu memory filesystem wlan battery date";
      cursor-click = "pointer";
      cursor-scroll = "ns-resize";
    };

    "module/backlight" = {
      type = "internal/xbacklight";
      format = "<label> <bar>";
      label = "💡";
      bar-width = 10;
      bar-indicator = "◎";
      bar-indicator-foreground = "\${colors.bar-foreground}";
      bar-indicator-font = 2;
      bar-fill = " ◦ ";
      bar-fill-font = 2;
      bar-fill-foreground = "\${colors.backlight-active}";
      bar-empty = " ◦ ";
      bar-empty-font = 2;
      bar-empty-foreground = "\${colors.backlight-inactive}";
    };

    "module/volume" = {
      type = "internal/volume";
      format-volume = "<label-volume> <bar-volume>";
      label-volume = "🔊";
      label-volume-foreground = "\${colors.volume-active}";
      format-muted = "<label-muted> <bar-volume>";
      label-muted = "🔇";
      format-muted-foreground = "\${colors.volume-inactive}";

      bar-volume-width = 10;
      bar-volume-foreground-0 = "\${colors.volume-active}";
      bar-volume-foreground-1 = "\${colors.volume-active}";
      bar-volume-foreground-2 = "\${colors.volume-active}";
      bar-volume-foreground-3 = "\${colors.volume-active}";
      bar-volume-foreground-4 = "\${colors.volume-active}";
      bar-volume-foreground-5 = "\${colors.volume-active}";
      bar-volume-foreground-6 = "\${colors.volume-active}";
      bar-volume-gradient = false;
      bar-volume-indicator = "◎";
      bar-volume-indicator-font = 2;
      bar-volume-fill = " ◦ ";
      bar-volume-fill-font = 2;
      bar-volume-empty = " ◦ ";
      bar-volume-empty-font = 2;
      bar-volume-empty-foreground = "\${colors.volume-inactive}";
    };

    "module/cpu" = {
      type = "internal/cpu";
      interval = 2;
      format = "⏲ <label>";
      label = "%percentage%%";
    };

    "module/memory" = {
      type = "internal/memory";
      interval = 2;
      format-prefix = "📟 ";
      format-prefix-foreground = "\${colors.bar-foreground}";
      label = "%percentage_used%%";
    };

    "module/filesystem" = {
      type = "internal/fs";
      interval = 30;
      mount-0 = "/";
      label-mounted = "💾 %percentage_used%%";
    };

    "module/wlan" = {
      type = "internal/network";
      interface = "wlp9s0";
      interval = 3;
      format-connected = "📶 <label-connected>";
      label-connected = "%essid%";
    };

    "module/battery" = {
      type = "internal/battery";
      battery = "BAT0";
      adapter = "ADP1";
      full-at = "98";
      format-charging-prefix = "🔌 ";
      format-charging = "<label-charging>";
      format-discharging-prefix = "🔋 ";
      format-discharging = "<label-discharging>";
      format-full-prefix = "⚡ ";
    };

    "module/date" = {
      type = "internal/date";
      interval = 5;
      date = "%B %d · %H:%M";
      label = "%date%";
    };
  };
}
