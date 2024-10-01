sudo nixos-rebuild dry-build -I nixos-config=./configuration.nix --show-trace &&
	git add . && git commit -m 'capturing new nixos config'
sudo nixos-rebuild switch -I nixos-config=./configuration.nix --show-trace &&
	git push
