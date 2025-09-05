{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs =
    { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        inherit (pkgs) lib;
      in
      {
        apps = {
          build = {
            type = "app";
            program = (
              lib.getExe (
                pkgs.writeShellApplication {
                  name = "build";
                  runtimeInputs = with pkgs; [
                    elmPackages.elm-format
                    git
                    python312
                  ];
                  text = ''
                    bash ./build.sh "" "$1"
                  '';
                }
              )
            );
          };
          build-on-new-version = {
            type = "app";
            program = (
              lib.getExe (
                pkgs.writeShellApplication {
                  name = "check-feed";
                  runtimeInputs = with pkgs; [
                    jq
                    jo
                  ];
                  text = ''
                    if test "$#" -lt 1 || test "$#" -gt 2; then
                      echo "Unexpected number of arguments: $#" >&2
                      exit 1
                    fi

                    if ! test -f "$1"; then
                      echo "Passed argument is not the file path of an existing file" >&2
                      exit 1
                    fi

                    newest_entry="$(jq -c '.entries[0]' "$1")"
                    new_timestamp="$(echo "$newest_entry" | jq '.published')"
                    version="$(echo "$newest_entry" | jq '.title')"
                    timestamp_file=''${2:-last_timestamp}
                    timestamp='0'
                    if test -f "$timestamp_file"; then
                      timestamp="$(cat "$timestamp_file")"
                    fi
                    is_new="false"
                    if test "$timestamp" != "$new_timestamp"; then
                      is_new="true"
                      timestamp="$new_timestamp"
                      echo "$new_timestamp" > "$timestamp_file"
                    fi

                    if test "$is_new" != "true"; then
                      echo "Not newer" >&2
                      exit 0
                    fi

                    nix run .#build -- "$version" 1>&2

                    jo is_new="$is_new" version="$version" timestamp="$timestamp"
                  '';
                }
              )
            );
          };
        };
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            python312
            elmPackages.elm-format
            git
          ];
        };
      }
    );
}
