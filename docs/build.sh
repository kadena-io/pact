#!/bin/bash

# --- Jump into Nix Shell --- #
nix-shell --pure -p 'python38.withPackages (ps: [ps.sphinx ps.sphinx_rtd_theme])' -p pandoc -p perl --run "./work.sh"
