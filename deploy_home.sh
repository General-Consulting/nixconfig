home-manager switch -n -f ./home.nix && \
  cp ./home.nix ~/.config/home-manager/
  git add . && \
  git commit -m 'capturing new home-manager config' && \
  home-manager switch  \
    &&\
  git push
