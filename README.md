# About

This is a unified repo for some of my configuration files.  A main idea is to consistenly use
- [Solarized](http://ethanschoonover.com/solarized) as a color scheme, mostly in its light variant
- [DejaVu Sans Mono](https://dejavu-fonts.github.io/) as a font, since looks good and has the best
  Unicode support of monospaced fonts, AFAIK (at least when it comes to strange operators in
  programming languages...)
  
It mostly stores my settings for zsh with OhMyZsh and Powerlevel9k and my emacs file. 


# Installations

## ZSH

See [here](https://github.com/robbyrussell/oh-my-zsh/wiki/Installing-ZSH).  To make it the default
shell: `chsh -s $(which zsh)`.

## OhMyZsh

See [here](https://github.com/robbyrussell/oh-my-zsh); basically, just

```
sh -c "$(wget https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"
```

For `zsh-completions`, giving [load of completions](https://github.com/zsh-users/zsh-completions),
also clone the following:

```
git clone https://github.com/zsh-users/zsh-completions ~/.oh-my-zsh/custom/plugins/zsh-completions
```

## Powerlevel9k

See [here](https://github.com/bhilburn/powerlevel9k):

```
git clone https://github.com/bhilburn/powerlevel9k.git ~/.oh-my-zsh/custom/themes/powerlevel9k
```

A list of all available prompt segments is given
[here](https://github.com/bhilburn/powerlevel9k#available-prompt-segments); my settings use
```
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(context dir custom_python vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status history background_jobs)
```

## Fonts

- DejaVu Sans Mono from [here](https://dejavu-fonts.github.io/)
- For terminals: the Powerline patched variant from [here](https://github.com/powerline/fonts) (I do
  that manually, but there's a script, too).
- Add _Awesome Terminal Fonts_ for additional fallback symbols: clone [the
  repo](https://github.com/gabrielelana/awesome-terminal-fonts) and run `install.sh`

## Hub

See [here](https://github.com/github/hub).  

- Installation (requires go to be installed)
```
git clone https://github.com/github/hub.git
cd hub
sudo make install prefix=/usr/local
```
- Add it to `PATH`: `ln -s /usr/local/hub-<version>/bin/hub /usr/local/bin/hub`
- Aliasing is done already in the `bashrc` and `zshrc` files
- Install [shell tab completion script](https://github.com/github/hub#shell-tab-completion) into `.oh-my-zsh/custom/plugins/zsh-completions/src/_hub`.

## Other tools

- [ ] [exa](https://github.com/ogham/exa)
- [ ] [fzf](https://github.com/junegunn/fzf), [fzf-zsh](https://github.com/Wyntau/fzf-zsh)

## Solarized for Xfce terminal

See [here](https://github.com/sgerrand/xfce4-terminal-colors-solarized); simply paste the file from
there into `~/.config/xfce4/terminal/terminalrc`.

Guake has several variants of the Solarized scheme built in, as has Gnome Terminal.

## Julia Packages

For the `.juliarc.jl`.  Depending on what you want to use, call
- [`Pkg.add("OhMyREPL")`](https://github.com/KristofferC/OhMyREPL.jl) (for improved REPL features)
- [`Pkg.add("Revise")`](https://github.com/timholy/Revise.jl) (for automatic module reloading)
in the Julia shell.

## Xmonad

- [Install xmonad using stack](https://xmonadhaskell.wordpress.com/2018/10/22/xmonad-ubuntu-18-04-install/)
- Install `dmenu`: `sudo apt-get install suckless-tools`
- [Install `xmobar`](https://xmonadhaskell.wordpress.com/2018/10/22/xmobar-ubuntu-18-04-install/): `stack install --flag xmobar:all_all_extensions xmobar`

[This blog](https://xmonadhaskell.wordpress.com/) and [this introduction](https://beginners-guide-to-xmonad.readthedocs.io/intro.html) is quite useful in general.
