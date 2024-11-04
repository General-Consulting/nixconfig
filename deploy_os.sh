# check if --no-upgrade is passed

if [[ "$1" == "--no-upgrade" ]]; then
	echo "Skipping upgrade"
	sudo nixos-rebuild dry-build -I nixos-config=./configuration.nix --show-trace &&
		git add . && git commit -m 'capturing new nixos config'
	sudo nixos-rebuild switch -I nixos-config=./configuration.nix --show-trace --upgrade &&
		git push
else
	sudo nix-channel --update
	sudo nixos-rebuild dry-build -I nixos-config=./configuration.nix --show-trace &&
		git add . && git commit -m 'capturing new nixos config'
	sudo nixos-rebuild switch -I nixos-config=./configuration.nix --show-trace --upgrade &&
		git push
fi
