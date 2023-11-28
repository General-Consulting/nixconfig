nixos-rebuild dry-build && git add . && git commit -m 'capturing new nixos config' \
&& sudo nixos-rebuild switch -I nixos-config=./configuration.nix \
&& git push
