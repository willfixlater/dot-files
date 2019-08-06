{ pkgs ? import <nixpkgs> {} }:

let
  baseEmacs = pkgs.emacs;
  emacsWithPackages = (pkgs.emacsPackagesNgGen baseEmacs).emacsWithPackages;
in
  emacsWithPackages (epkgs:
    (with epkgs.melpaStablePackages; [
      better-defaults
      color-theme-sanityinc-tomorrow
      emojify
      ag
      ace-window
      paredit
      projectile
      magit
      # TODO: Get magit-gitflow packages to build correctly
      # Error appears similar to https://github.com/NixOS/nixpkgs/issues/45868
      # magit-gitflow
      flycheck
      company
      nix-mode
      markdown-mode
      haml-mode
      sass-mode
      js2-mode
      rjsx-mode
      clojure-mode
      cider
      clj-refactor
      geiser
      haskell-mode
      rust-mode
      elpy
      yaml-mode
    ])
    ++
    (with epkgs.melpaPackages; [
      restclient
      floobits
    ])
    ++
    (with epkgs.elpaPackages; [])
    ++
    [ ]
  )
