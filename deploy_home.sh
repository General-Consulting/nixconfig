# Ensuring the script exits if an unhandled error occurs
set -eo pipefail

# Define variables for directories to enhance readability and maintainability
HOME_NIX_SOURCE="./home.nix"
HOME_NIX_DEST="../.config/home-manager/"
DOTFILES_SOURCE="./dotfiles/"
DOTFILES_DEST="../.config/home-manager/dotfiles"

# Check if the configuration is correct
# This assumes `home-manager` can validate the configuration without applying it.
# Adjust the command if your setup requires a different way to validate.
echo "Checking configuration correctness..."
if ! home-manager switch -n; then
	echo "Configuration check failed. Exiting script."
	exit 1
fi
echo "Configuration is correct."

# Prompting the user for a commit message with a default option
read -p "Enter commit message [Capturing new home-manager config]: " commit_msg
commit_msg=${commit_msg:-'Capturing new home-manager config'}

# Updating home.nix configuration
echo "Updating home.nix configuration..."
ln -sf $PWD/$HOME_NIX_SOURCE $HOME_NIX_DEST/home.nix

echo "Updating dotfiles..."
# Safely remove and copy the new dotfiles
[ -d "$DOTFILES_DEST" ] && rm -rf "$DOTFILES_DEST"
cp -r $DOTFILES_SOURCE $DOTFILES_DEST

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
