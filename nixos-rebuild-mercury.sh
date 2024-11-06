#!/usr/bin/env bash

sudo nixos-rebuild switch --flake .#mercury
xmonad --recompile
xmonad --restart
