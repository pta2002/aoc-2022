{
  description = "Advent of code 2022";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system}; in
      {
        devShell = with pkgs; mkShell {
          name = "aoc2022";
          buildInputs = [
            haskell-language-server

            rustc
            cargo
            rust-analyzer
            rustfmt

            (haskell.packages.ghc924.ghcWithPackages (p: with p; [
              megaparsec
              split
              heap
              search-algorithms
            ]))
          ];
        };
      });
}
