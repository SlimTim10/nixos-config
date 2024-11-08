#!/usr/bin/env bash

sudo nixos-rebuild switch --flake .#laptop && xmonad --recompile && xmonad --restart
