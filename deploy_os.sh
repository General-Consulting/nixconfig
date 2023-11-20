git add . && git commit -m 'capturing new nixos config'
nixos-rebuild dry-build && sudo nixos-rebuild switch -I nixos-config=./configuration.nix
