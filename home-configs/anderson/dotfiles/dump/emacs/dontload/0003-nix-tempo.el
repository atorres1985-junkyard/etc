;;; -*- lexical-binding: t; no-byte-compile: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nix/Nixpkgs Tempo macros ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar nix-tempo-tags nil
  "Tempo tags for nix-mode.")

(defun nix-tempo-settings ()
  (tempo-use-tag-list 'nix-tempo-tags)
  (atorres1985|tempo-keyboard-macros))

(add-hook 'nix-mode-hook '(lambda () (nix-tempo-settings)))

(tempo-define-template "nixpkgs-meta"
                       '("meta = {" > n
                         "homepage = \"" (p "homepage: ") "\";" > n
                         "description = \"" (p "description: ") "\";" > n
                         "license = with lib.licenses; [ " (p "licenses: ") " ];" > n
                         "maintainers = with lib.maintainers; [ " (p "maintainers: ") " ];" > n
                         "platforms = lib.platforms." (p "platforms: ") ";" > n
                         "};" > n)
                       "Tnixmeta"
                       "Insert a basic meta."
                       'nix-tempo-tags)

(tempo-define-template "nixpkgs-fetchurl"
                       '("fetchurl {" > n
                         "url = \"" (p "URL: " url) "\";" > n
                         "hash = \"\";" > n
                         "};" > n)
                       "Tnixfetchurl"
                       "Insert a basic fetchurl."
                       'nix-tempo-tags)

(tempo-define-template "nixpkgs-fetchfromgithub"
                       '("fetchFromGitHub {" > n
                         "owner = \"" (p "Owner: " owner) "\";" > n
                         "repo = \"" (p "Repo: " repo) "\";" > n
                         "rev = " (p "Rev: " rev) ";" > n
                         "hash = \"\";" > n
                         "};" > n)
                       "Tnixfetchfromgithub"
                       "Insert a basic fetcher for github."
                       'nix-tempo-tags)

(tempo-define-template "nixpkgs-mkderivation"
                       '("{ lib" > n
                         ", stdenv" > n
                         "}:" > n
                         n
                         "stdenv.mkDerivation (finalAttrs: {" > n
                         "pname = \"" (p "pname: " pname) "\";" > n
                         "version = \"" (p "version: " version) "\";" > n
                         "})" > n)
                       "Tnixmkderivation"
                       "Insert a basic stdenv.mkDerivation."
                       'nix-tempo-tags)

(tempo-define-template "nixpkgs-setuphook"
                       '("{ lib" > n
                         ", makeSetupHook" > n
                         ", " (p "Input: " input) > n
                         "}:" > n
                         n
                         "makeSetupHook {" > n
                         "name = \"" (p "Name (without -hook suffix): ") "-hook\";" > n
                         n
                         "propagatedBuildInputs = [ " input " ];" > n
                         n
                         "passthru = {" > n
                         "inherit " input ";" > n
                         "};" > n
                         n
                         "meta = {" > n
                         "description = \"A setup hook for" input "\";" > n
                         "inherit (" input ".meta) maintainers platforms broken;"
                         "};" > n
                         "} ./setup-hook.sh" > n)
                       "Tnixsetuphook"
                       "Insert a basic makeSetupHook."
                       'nix-tempo-tags)
