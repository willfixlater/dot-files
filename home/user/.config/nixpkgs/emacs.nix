{ pkgs ? import <nixpkgs> {} }:

let
  baseEmacs = pkgs.emacs;
  emacsWithPackages = (pkgs.emacsPackagesNgGen baseEmacs).emacsWithPackages;
in
  emacsWithPackages (epkgs:
    (with epkgs.melpaStablePackages; [
      use-package
      better-defaults
      color-theme-sanityinc-tomorrow
      emojify
      ag
      ace-window
      projectile
      magit
      # TODO: Get magit-gitflow packages to build correctly
      # Error appears similar to https://github.com/NixOS/nixpkgs/issues/45868
      # magit-gitflow
      flycheck
      company
      yasnippet
      # ---
      paredit
      eglot
      nix-mode
      markdown-mode
      haml-mode
      sass-mode
      js2-mode
      rjsx-mode
      clojure-mode
      cider
      inf-clojure
      clj-refactor
      geiser
      haskell-mode
      rust-mode
      elpy
      yaml-mode
      dart-mode
    ])
    ++
    (with epkgs.melpaPackages; [
      restclient
      floobits
      flutter
    ])
    ++
    (with epkgs.elpaPackages; [])
    ++
    [ ]
  )
