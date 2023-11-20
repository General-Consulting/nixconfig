git add . && git commit -m 'capturing new nixos config'
nix-rebuild dry-build && sudo nixos-rebuild switch -I nixos-config=./configuration.nix
