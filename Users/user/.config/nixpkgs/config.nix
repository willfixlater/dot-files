# TODO
# - Flutter (not currently possible via nix)
# External Requirements:
# - emacs: Fira Code font is available on system.
# - postgresql: Directory `~/.postgres` must exist and user must have read-write
# access to it.
# - postgresql: To init cluster, run the following
# `pg_ctl init -D .postgres/data -o --no-locale`.
# - postgresql: Directory `/run/postgresql` must exist and user must have
# read-write access to it.
# - postgresql: To start the postgresql daemon, run the following
# `pg_ctl -D .postgres/data -l .postgres/log start`
{ pkgs }:

{
  allowUnfree = true;
  packageOverrides = super:
    let pkgs = super.pkgs;
    in with pkgs; rec {
      my-env = pkgs.buildEnv {
        name = "my-env";
        paths = [
          git
          gitAndTools.gitflow
          openssh
          ag
          postgresql
          netcat-gnu
          rlwrap
          tmux
          stack
          awscli
          google-cloud-sdk
          #mailcatcher
          (import ./emacs.nix { inherit pkgs; })
          # Flutter
          #vscode
          #android-studio
          # Clojure
          leiningen
          # Node
          nodejs
        ];
      };
    };
}
