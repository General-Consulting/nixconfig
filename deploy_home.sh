# Ensuring the script exits if an unhandled error occurs
set -eo pipefail

# Define variables for directories to enhance readability and maintainability
HOME_NIX_SOURCE="./home.nix"
HOME_NIX_DEST="$HOME/.config/home-manager"
DOTFILES_SOURCE="$PWD/dotfiles/"
DOTFILES_DEST="$HOME/.config/home-manager/dotfiles"

[ -d "$HOME_NIX_DEST" ] && rm -r "$HOME_NIX_DEST"
ln -s $PWD $HOME_NIX_DEST

# Check if the Neovim configuration is correct
echo "Checking Neovim configuration correctness..."
if ! nvim --headless -u "$NVIM_CONFIG_SOURCE" -c "quit"; then
	echo "Neovim configuration check failed. Exiting script."
	exit 1
fi
echo "Neovim configuration is correct."

# Check if the configuration is correct
# This assumes `home-manager` can validate the configuration without applying it.
# Adjust the command if your setup requires a different way to validate.
echo "Home manager configuration correctness..."
if ! home-manager switch -n --show-trace; then
	echo "Configuration check failed. Exiting script."
	exit 1
fi
echo "Home manager Configuration is correct."

# Prompting the user for a commit message with a default option
read -p "Enter commit message [Capturing new home-manager config]: " commit_msg
commit_msg=${commit_msg:-'Capturing new home-manager config'}

# Proceed with git operations
echo "Committing changes to Git with message: '$commit_msg'..."
git add .
git commit -m "$commit_msg"

# Apply the actual home-manager configuration changes
echo "Applying home-manager changes..."
home-manager switch

# Push the changes to the remote repository
echo "Pushing changes to remote Git repository..."
git push

echo "Script completed successfully!"
