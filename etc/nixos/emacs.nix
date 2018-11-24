{ pkgs ? import <nixpkgs> {} }:

let
  baseEmacs = pkgs.emacs;
  emacsWithPackages = (pkgs.emacsPackagesNgGen baseEmacs).emacsWithPackages;
in
  emacsWithPackages (epkgs:
    (with epkgs.melpaStablePackages; [
      better-defaults
      color-theme-sanityinc-tomorrow
      ag
      ace-window
      paredit
      projectile
      geiser
      magit
      # TODO: Get magit-gitflow packages to build correctly
      # Error appears similar to https://github.com/NixOS/nixpkgs/issues/45868
      # magit-gitflow
      nix-mode
      markdown-mode
      haml-mode
      sass-mode
      js2-mode
      rjsx-mode
      clojure-mode
      cider
      clj-refactor
      haskell-mode
      rust-mode
    ])
    ++
    (with epkgs.melpaPackages; [ ])
    ++
    (with epkgs.elpaPackages; [ ])
    ++
    [ ]
  )
