;;; -*- lexical-binding: t; no-byte-compile: t -*-

(define-skeleton skel-nixpkgs-mkderivation
  "Skeleton for a typical mkDerivation."
  nil
  "{ lib" > \n
  ", stdenv" > \n
  "}:" > \n
  "" > \n
  "stdenv.mkDerivation (finalAttrs: {" > \n
  "pname = \"" (skeleton-read "pname: ") "\";" > \n
  "version = \"" (skeleton-read "version: ") "\";" > \n
  "" > \n
  "})" > \n)

(define-skeleton skel-nixpkgs-src-fetchurl
  "Skeleton for a typical fetchurl."
  nil
  "src = fetchurl {" > "\n"
  "url = " ?\" (skeleton-read "URL: ") ?\" ";" > "\n"
  "hash = " ?\" (skeleton-read "hash: ") | "" ?\" ";" > "\n"
  "};" > "\n")

(define-skeleton skel-nixpkgs-src-github
  "Skeleton for a typical fetchFromGitHub."
  nil
  "src = fetchFromGitHub {" > "\n"
  "owner = " ?\" (skeleton-read "owner: ") ?\" ";" > "\n"
  "repo = " ?\" (skeleton-read "repo: ") ?\" ";" > "\n"
  "rev = " ?\" (skeleton-read "rev: ") ?\" ";" > "\n"
  "hash = " ?\" (skeleton-read "hash: ") | "" ?\" ";" > "\n"
  "};" > "\n")

(define-skeleton skel-nixpkgs-src-fetchFrom
  "Skeleton for a typical fetchFrom-like function."
  nil
  "src = fetchFrom" (skeleton-read "fetcher: ") " {" > "\n"
  "owner = " ?\" (skeleton-read "owner: ") ?\" ";" > "\n"
  "repo = " ?\" (skeleton-read "repo: ") ?\" ";" > "\n"
  "rev = " ?\" (skeleton-read "rev: ") ?\" ";" > "\n"
  "hash = " ?\" (skeleton-read "hash: ") | "" ?\" ";" > "\n"
  "};" > "\n")

(define-skeleton skel-nixpkgs-meta
  "Skeleton for a typical meta."
  nil
  "meta = {" > "\n"
  "homepage = " ?\" (skeleton-read "homepage: ") ?\" ";" > "\n"
  "description = " ?\" (skeleton-read "description: ") ?\" ";" > "\n"
  "license = lib.licenses." (skeleton-read "license: ") ";" > "\n"
  "maintainers = with lib.maintainers; [ " (skeleton-read "maintainers: ") " ];" > "\n"
  "platforms = with lib.platforms; " (skeleton-read "platforms: ") ";" > "\n"
  "};" > \n)
