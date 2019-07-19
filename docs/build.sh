#!/bin/bash

# --- Jump into Nix Shell --- #
nix-shell --pure -p python3 python37Packages.sphinx python37Packages.sphinx_rtd_theme pandoc perl --run "./work.sh"
