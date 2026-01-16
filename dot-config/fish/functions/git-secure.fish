#!/usr/bin/env fish

function _git-secure -a name email key -d "set local config to used signed keys for all commits"
  argparse --name=git-secure 'h/help' 'n/name=' 'e/email=' 'k/key=' 'f/format=' -- $argv
  
  if set -q _flag_help
    echo "Usage: git secure -n NAME -e EMAIL -k KEY [-f FORMAT]"
    echo ""
    echo "Configure local git repo to sign all commits with an SSH key."
    echo ""
    echo "Options:"
    echo "  -n, --name=NAME      Author name for commits"
    echo "  -e, --email=EMAIL    Author email for commits (required)"
    echo "  -k, --key=KEY        Path to private signing key (required)"
    echo "  -f, --format=FORMAT  Signature format (default: ssh)"
    echo "  -h, --help           Show this help message"
    echo ""
    echo "Examples:"
    echo "  git secure -n \"John Doe\" -e john.doe@example.com -k ~/.ssh/id_ed25519"
    echo "  git secure -e john.doe@example.com -k ~/.ssh/id_ed25519  # uses global name"
    echo ""
    echo "Notes:"
    echo "  - The public key (KEY.pub) must exist alongside the private key"
    echo "  - Directory ~/.config/git must exist for allowed_signers file"
    return 0
  end

  if not string match -q "true" (git rev-parse --is-inside-work-tree 2>/dev/null)
    echo "ERROR: git-secure must be run in the working tree of a git repo"
    return 1
  end

  # Set defaults
  set -l name $_flag_name
  set -l format $_flag_format

  if test -z "$name"
    set name (git config --global user.name)
  end

  if test -z "$format"
    set format "ssh"
  end

  if test -z "$name"; or test -z "$_flag_email"; or test -z "$_flag_key"
    echo "ERROR: please provide NAME (or set global git user.name), EMAIL, and KEY"
    return 1
  end

  set -l public_key "$_flag_key.pub"
  set -l allowed_signers_file ~/.config/git/allowed_signers

  if not test -f "$_flag_key"
    echo "ERROR: private key file $_flag_key does not exist"
    return 1
  end

  if not test -f "$public_key"
    echo "ERROR: public key file $public_key does not exist"
    return 1
  end

  if not test -d (dirname $allowed_signers_file)
    echo "ERROR: directory $(dirname $allowed_signers_file) does not exist"
    return 1
  end

  git config --local user.name "$name"
  git config --local user.email "$_flag_email"
  git config --local user.signingkey "$_flag_key"
  git config --local commit.gpgsign true
  git config --local gpg.format "$format"
  git config --local gpg.ssh.allowedsignersfile $allowed_signers_file

  # Append key if not already present
  set -l pubkey_content (cat $public_key)
  if not grep -qF "$pubkey_content" $allowed_signers_file 2>/dev/null
    echo "$_flag_email $pubkey_content" >> $allowed_signers_file
    echo "public key added to $allowed_signers_file"
  else
    echo "public key already exists in $allowed_signers_file"
  end

  set -l cfg_name       (git config get user.name)
  set -l cfg_email      (git config get user.email)
  set -l cfg_signingkey (git config get user.signingkey)
  set -l cfg_gpgsign    (git config get commit.gpgsign)
  set -l cfg_gpgformat  (git config get gpg.format)

  echo ""
  echo "updated git config user.name to \"$cfg_name\", user.email to \"$cfg_email\", user.signingkey to \"$cfg_signingkey\", commit.gpgsign to $cfg_gpgsign and gpg.format to $cfg_gpgformat"
end

_git-secure $argv
