let
  fetch = package:
    let
      versions = builtins.fromJSON (builtins.readFile ./versions.json);
      spec = versions.${package};
      fetchTarball =
        # fetchTarball version that is compatible between all the versions of
        # Nix
        { url, sha256 }@attrs:
        let
          inherit (builtins) lessThan nixVersion fetchTarball;
        in
          if lessThan nixVersion "1.12" then
            fetchTarball { inherit url; }
          else
            fetchTarball attrs;
    in
      fetchTarball {
        url =
          with spec;
          "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
        sha256 = spec.sha256;
      };
in { nixpkgs ? fetch "nixpkgs" }: import nixpkgs {
  config = {};
  overlays =
    [
      # CIO
      (self: super:
        { haskellPackages =
            super.haskellPackages.extend
              (super.haskell.lib.packageSourceOverrides
                { cio = self.lib.cleanSource ../cio; }
              );
        }
      )

      # IHaskell, if needed
      (self: super:
        { ihaskellWithPackages = ps:
            self.callPackage
              "${self.path}/pkgs/development/tools/haskell/ihaskell/wrapper.nix"
              {
                ghcWithPackages = self.haskellPackages.ghcWithPackages;
                jupyter = self.python3.withPackages (ps: [ ps.jupyter ps.notebook ]);
                packages = ps;
              };
        }
      )
    ];
  }
