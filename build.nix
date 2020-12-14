{ stdenv, lib, fetchurl, runCommand, makeWrapper, haskellPackages, hlint
, stylish-haskell, haskell-language-server, ormolu, development ? false }:
let
  ghc = haskellPackages.ghcWithPackages (hpkgs:
    with hpkgs;
    [ relude ] ++ lib.optionals development [ apply-refact retrie ]);

  hlintReludeYaml = fetchurl {
    url =
      "https://raw.githubusercontent.com/kowainik/relude/61d85ea3421de8bf06f797ff7b5b16bdd47733fa/.hlint.yaml";
    name = "hlint.yaml";
    sha256 = "14gbdl2m3hvhvf7sxza9bd12wv5b4wvj1imk6j75kv3ax9llk0di";
  };

  hlintRelude =
    runCommand "hlint-relude" { nativeBuildInputs = [ makeWrapper ]; } ''
      makeWrapper ${hlint}/bin/hlint $out/bin/hlint-relude \
        --add-flags --hint=${
          fetchurl {
            url =
              "https://raw.githubusercontent.com/kowainik/relude/61d85ea3421de8bf06f797ff7b5b16bdd47733fa/.hlint.yaml";
            name = "hlint.yaml";
            sha256 = "14gbdl2m3hvhvf7sxza9bd12wv5b4wvj1imk6j75kv3ax9llk0di";
          }
        }
    '';

in stdenv.mkDerivation ({
  pname = "advent-of-code-2020";
  version = "0.0.1";
  src = ./.;
  nativeBuildInputs = lib.optionals development [
    hlint
    hlintRelude
    stylish-haskell
    haskell-language-server
    ormolu
  ];
  buildInputs = [ ghc ];
} // lib.optionalAttrs development { RELUDE_HINT = hlintReludeYaml; })
