{ pkgs, ... }:

{
  enable = true;
  font = "Fira Code 18";
  lines = 5;
  borderWidth = 0;
  scrollbar = false;
  colors = {
    window = {
      background = "argb:ff141516";
      border = "argb:00000000";
      separator = "argb:00000000";
    };
    rows = {
      normal = {
        background = "argb:00141516";
        foreground = "argb:ffa3a5a5";
        backgroundAlt = "argb:00212222";
        highlight = {
          background = "argb:00141516";
          foreground = "argb:ffffffff";
        };
      };
      active = {
        background = "argb:ff545657";
        foreground = "argb:ffa3a5a5";
        backgroundAlt = "argb:ff545657";
        highlight = {
          background = "argb:ff545657";
          foreground = "argb:ffffffff";
        };
      };
      urgent = {
        background = "argb:00141516";
        foreground = "argb:ffcc6666";
        backgroundAlt = "argb:ff212222";
        highlight = {
          background = "argb:ffcc6666";
          foreground = "argb:ffffffff";
        };
      };
    };
  };
}
