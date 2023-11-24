# this probably can be sym linked
cp  ./home.nix ../.config/home-manager/ && \
rm -r ../.config/home-manager/dotfiles && cp -r ./dotfiles/ ../.config/home-manager/ &&\
home-manager switch -n && \
  git add . && \
  git commit -m 'capturing new home-manager config' && \
  home-manager switch  \
    &&\
  git push
