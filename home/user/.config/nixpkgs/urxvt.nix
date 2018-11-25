{ pkgs, ... }:

{
  enable = true;
  scroll.bar.enable = false;
  fonts = [
    "xft:Fira Code:bold:antialias=true:pixelsize=14"
  ];
  extraConfig = {
    cursorBlink = 1;
    cursorUnderline = 1;
    internalBorder = 18;
  };
}
