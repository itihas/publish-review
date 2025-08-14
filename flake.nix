{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs";
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.emacs-overlay.url = "github:nix-community/emacs-overlay";

  outputs = inputs@{ ... }: {
    flakeModules.default = { self, lib, ... }: {
      options.flake = with lib; {
        publishSrc = mkOption { type = types.path; };
        publishTarget = mkOption { type = types.string; };
      };

      config = {
        systems = [ "x86_64-linux" ];
        perSystem = { self', inputs', pkgs, system, ... }: {

          devShells.default = pkgs.mkShell {
            packages = [ self'.packages.buildEmacs ];
            shellHook = ''
              alias emacs="emacs --load ${./publish-review.el} --quick"
            '';
          };

          apps.default = {
            type = "app";
            program = self'.packages.push;
          };

          packages = {
            buildEmacs =
              inputs.emacs-overlay.lib.${system}.emacsWithPackagesFromUsePackage {
                package =
                  inputs.emacs-overlay.packages.${system}.emacs-unstable-nox;
                config = ./publish-review.el;
              };
            default = self'.packages.site;
            push = pkgs.writeShellScriptBin "push" ''
              rm -rf /tmp/review
              cd /tmp
              git clone ${self.publishTarget} --depth=1
              cd review
              ${pkgs.rsync}/bin/rsync --verbose --recursive ${self'.packages.site}/ ./
              chown -R  $USER:users .
              git status
              git config user.email "flake-deploy@localhost"
              git config user.name "flake-deploy"
              git add .
              git commit -m "flake build"
              git push
            '';
            site = pkgs.stdenv.mkDerivation {
              name = "site";
              src = self.publishSrc;
              buildInputs = [ self'.packages.buildEmacs ];
              buildPhase = ''
                export ORGDIR=$PWD
                emacs --load ${
                  ./publish-review.el
                } --batch --eval "(publish-itihas-review)"
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
