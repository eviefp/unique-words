let
  tooling = import ./nix/default.nix;
  pkgs = tooling.pkgs;
  self = tooling.haskell.ghc8102;
in
  pkgs.mkShell {
    buildInputs = self.defaultInputs ++ [ pkgs.zlib pkgs.llvm_9 ];
  }
