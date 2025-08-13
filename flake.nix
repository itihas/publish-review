{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs";
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";

  outputs = { self, nixpkgs, ... }: {
    flakeModules.default = {
      options.src = with nixpkgs.lib; mkOption { type = types.path; };
      config = {
        systems = [ "x86_64-linux" ];
        perSystem = { inputs, pkgs, system, ... }: {
          packages = let pkgs = nixpkgs.legacyPackages."x86_64-linux";
          in rec {
            default = push;
            push = pkgs.writeShellScriptBin "push-review" ''
              rm -rf /tmp/review
              cd /tmp
              git clone git@github.com:itihas/review --depth=1
              cd review
              ${pkgs.rsync}/bin/rsync --verbose --recursive ${
                self.packages."x86_64-linux".build
              }/ ./
              chown -R  $USER:users .
              git status
              git config user.email "flake-deploy@localhost"
              git config user.name "flake-deploy"
              git add .
              git commit -m "flake build"
              git push
            '';
            build = pkgs.stdenv.mkDerivation {
              name = "itihas-review";
              src = self.src;
              buildInputs = [
                (pkgs.emacs-nox.pkgs.withPackages
                  (epkgs: with epkgs; [ org citar org-roam org-roam-bibtex ]))
              ];
              buildPhase = ''
                emacs --load ${
                  ./publish-review.el
                } --quick --batch --eval "(setq org-directory \"$src\")" --execute "(publish-itihas-review)"
              '';
              installPhase = ''
                mkdir -p $out
                cp -R out/*.html $out/
              '';
            };
          };
        };
      };
    };
  };
}
