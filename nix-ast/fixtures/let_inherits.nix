x: let
  inherit (x) y;
  inherit x;
  z = 7;
in y + z + x.y
