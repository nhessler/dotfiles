# SSH Setup

SSH keys are set up manually since they're machine-specific and require careful handling.

## Key Naming Convention

Keys follow the pattern: `id_<algorithm>.<service>.<account>.<purpose>`

Examples:
- `id_ed25519.github.personal.auth` - GitHub personal account, authentication
- `id_ed25519.github.personal.sign` - GitHub personal account, commit signing
- `id_ed25519.github.work.auth` - GitHub work account, authentication
- `id_ed25519.aws.myproject` - AWS access for a specific project
- `id_ed25519.gigalixir.myapp` - Gigalixir deployment access

## Generating a Key

```shell
ssh-keygen -t ed25519 -f ~/.ssh/id_ed25519.<service>.<account>.<purpose>
```

Use a passphrase when prompted.

Reference: https://docs.github.com/en/authentication/connecting-to-github-with-ssh/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent

## SSH Config Structure

Edit `~/.ssh/config`:

```
# Global settings
Host *
  AddKeysToAgent yes
  UseKeychain yes

# GitHub - personal account
Host github.com
  Hostname github.com
  User git
  IdentityFile ~/.ssh/id_ed25519.github.personal.auth

# GitHub - work account (use custom host alias)
Host work.github.com
  Hostname github.com
  User git
  IdentityFile ~/.ssh/id_ed25519.github.work.auth

# Other services follow the same pattern
Host ssh.example.com
  Hostname ssh.example.com
  IdentityFile ~/.ssh/id_ed25519.example.myaccount
```

## Multiple GitHub Accounts

When using multiple GitHub accounts, create a host alias for non-default accounts.

Then clone repos with the alias:
```shell
# Personal (default)
git clone git@github.com:username/repo.git

# Work account
git clone git@work.github.com:org/repo.git
```

## Adding Keys to GitHub

1. Copy the public key:
   ```shell
   cat ~/.ssh/id_ed25519.github.personal.auth.pub | pbcopy
   ```

2. Add at https://github.com/settings/keys

## Verifying Setup

```shell
ssh -T git@github.com
```

Should see: "Hi username! You've successfully authenticated..."
