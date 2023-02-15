#!/bin/bash

# --- Jump into Nix Shell --- #
nix-shell --pure -p python3 python38Packages.sphinx python38Packages.sphinx_rtd_theme pandoc perl --run "./work.sh"
