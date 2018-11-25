{ pkgs, ... }:

{
  enable = true;
  shadow = true;
  noDockShadow = true;
  shadowOffsets = [ (-6) (-6) ];
  extraOptions = ''
    clear-shadow = true;
      shadow-radius = 4;
  '';
}
