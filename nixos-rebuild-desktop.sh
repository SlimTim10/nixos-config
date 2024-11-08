#!/usr/bin/env bash

sudo nixos-rebuild switch --flake .#desktop && xmonad --recompile && xmonad --restart
