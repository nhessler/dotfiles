Dotfiles
--------

## Summary

These are my dotfiles. There are many like them, but these or mine.

## Prerequisites
* Mac Setup
* Apple Account Setup
* [Github SSH Key Setup](https://help.github.com/articles/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent/)

## Bootstrap

Get the bare minimum going to support a more complete install of all the things. This should only need to be run when first getting a machine setup. To that end, once baked, it shouldn't change much

```shell
curl -fsSL https://raw.githubusercontent.com/nhessler/dotfiles/master/bootstrap.sh | sh
```

* Install Command Line Tools
* Install Software Updates
* Install Homebrew
* Install ASIDE
* Setup Project Space
* Validate necessary security keys exist
  - Github
  - Heroku

## Setup

Idempotently setup, install, and deploy all the things. This will need to be run regularly as it will be

```shell
cd ~/projects/nhessler/dotfiles && setup.sh | sh
```

* Set MacOS Defaults
* Install Mac Apps
* Install Command Line Tools
* Deploy Configs

## Inspirations
* [Strap](https://github.com/MikeMcQuaid/strap)
* [Github Does Dotfiles](https://dotfiles.github.io/)
