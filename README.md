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
* Install ASDF
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


## Password Security
* [Diceware](https://theworld.com/~reinhold/diceware.html)
* [EFF](https://www.eff.org/deeplinks/2016/07/new-wordlists-random-passphrases)
* Wordlists
* * [EFF 5](https://www.eff.org/files/2016/07/18/eff_large_wordlist.txt)
* * [EFF 4](https://www.eff.org/files/2016/09/08/eff_short_wordlist_1.txt)
* * [Beale](https://theworld.com/~reinhold/beale.wordlist.asc)
* * [Diceware](https://theworld.com/~reinhold/diceware.wordlist.asc)
* * [Norvig](https://norvig.com/ngrams/count_1w.txt)
