backup_file="../.config/home-manager/home.nix"
backup_dir="./"
backup_base_name="home.nix.host.backup"
backup_number=0

function backup() {
  while [ -f "${backup_dir}/${backup_base_name}.${backup_number}" ]; do
      backup_number=$((backup_number + 1))
  done

  output_file="${backup_dir}/${backup_base_name}.${backup_number}"

  cp "$backup_file" "$output_file"
}


    backup && \
cp  ./home.nix ../.config/home-manager/ && \
rm -r ../.config/home-manager/dotfiles && cp -r ./dotfiles/ ../.config/home-manager/ &&\
home-manager switch -n && \
  git add . && \
  git commit -m 'capturing new home-manager config' && \
  home-manager switch  \
    &&\
  git push
