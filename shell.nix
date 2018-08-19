{ with-jupyter ? true }:
let
  pkgs = import ./nix {};
  ROOT = builtins.toString ./.;
  ihaskell = pkgs.ihaskellWithPackages (ps:
    [ ps.cio ps.conduit ps.conduit-combinators ]);
  functions =
    [
      ''
        cio_ghci() {
          pushd ${ROOT}/cio
          cabal new-repl
          popd
        }

        cio_build() {
          pushd ${ROOT}/cio
          cabal new-build
          popd
        }
      ''
    ] ++ pkgs.lib.optional with-jupyter
      ''
        cio_notebook() {
          ihaskell-notebook
        }

        cio_readme_gen() {
          jupyter-nbconvert \
            --to markdown \
            ${ROOT}/README.ipynb \
            --stdout \
            > README.md
        }
      '';
in pkgs.haskellPackages.shellFor
  {
    packages = p: [ p.cio ];
    withHoogle = false;
    buildInputs =
      [ pkgs.cabal-install ] ++ pkgs.lib.optional with-jupyter ihaskell ;
    shellHook =
      pkgs.lib.strings.concatStringsSep "\n"
      (
      [''local pre_functions=$(declare -F | cut -d" " -f3-)'']
      ++ functions ++
      [''
        local post_functions=$(declare -F | cut -d" " -f3-)

        echo "helpers:"
        diff <(echo "$pre_functions") <(echo "$post_functions") | grep '> '

      '']
      );
  }
