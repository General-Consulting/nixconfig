home-manager switch -n -I ./home.nix &&  git add . && git commit -m 'capturing new nixos config' \
&& home-manager switch -I config=./configuration.nix 
git push
