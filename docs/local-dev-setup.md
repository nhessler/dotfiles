# Local Development Setup

This documents the setup for local development with Caddy (reverse proxy), dnsmasq (local DNS), and macOS resolver.

## Overview

This setup allows you to:
- Use custom local domains (e.g., `myapp.test`) instead of `localhost:3000`
- Automatic HTTPS with local certificates
- Route multiple local apps through one entry point

## Directory Structure

```
~/Projects/nhessler/dotfiles/
└── caddy/
    └── Caddyfile    # Machine-specific, gitignored
```

The Caddyfile is symlinked to Homebrew's config location:
```
/opt/homebrew/etc/Caddyfile -> ~/Projects/nhessler/dotfiles/caddy/Caddyfile
```

## Caddy Setup

### 1. Create Caddyfile

```shell
mkdir -p ~/Projects/nhessler/dotfiles/caddy
touch ~/Projects/nhessler/dotfiles/caddy/Caddyfile
```

### 2. Symlink to Homebrew location

```shell
ln -sf ~/Projects/nhessler/dotfiles/caddy/Caddyfile /opt/homebrew/etc/Caddyfile
```

### 3. Example Caddyfile

```
myapp.test {
  reverse_proxy localhost:4000
}

api.test {
  reverse_proxy localhost:3000
}
```

### 4. Start Caddy

```shell
brew services start caddy
```

## dnsmasq Setup

dnsmasq provides local DNS resolution for `.test` domains.

### 1. Configure dnsmasq

Edit `/opt/homebrew/etc/dnsmasq.conf`:

```
# Route all .test domains to localhost
address=/test/127.0.0.1
```

### 2. Start dnsmasq

```shell
sudo brew services start dnsmasq
```

## macOS Resolver Setup

Tell macOS to use dnsmasq for `.test` domains.

### 1. Create resolver directory

```shell
sudo mkdir -p /etc/resolver
```

### 2. Create resolver file

```shell
sudo tee /etc/resolver/test << EOF
nameserver 127.0.0.1
EOF
```

### 3. Verify

```shell
scutil --dns | grep -A 3 "resolver #"
```

Should show a resolver for `.test` pointing to `127.0.0.1`.

## Testing the Setup

1. Add an entry to your Caddyfile
2. Reload Caddy: `brew services restart caddy`
3. Start your app on the configured port
4. Visit `https://myapp.test` in your browser

## Troubleshooting

### DNS not resolving
```shell
# Check dnsmasq is running
brew services list | grep dnsmasq

# Test DNS resolution
dig myapp.test @127.0.0.1
```

### Caddy not proxying
```shell
# Check Caddy status
brew services list | grep caddy

# Check Caddy logs
tail -f /opt/homebrew/var/log/caddy.log
```

### Certificate issues
Caddy auto-generates local certificates. If browsers complain:
```shell
caddy trust
```
