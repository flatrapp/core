let
  fetchNixpkgs = { rev, sha256 } : builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };
in
  import (fetchNixpkgs {
    rev = "5141f28405e5d31f21c10869dfc86ff340053787";
    sha256 = "0q91kfxg950g1nr71ifxhb4gfn3vfs4szh2yn7z8s2xri4l36p5m";
  })
