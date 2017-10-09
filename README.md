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

## Powerlevel9k

See [here](git clone https://github.com/bhilburn/powerlevel9k.git ~/.oh-my-zsh/custom/themes/powerlevel9k):

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

- Installation:
```
git clone https://github.com/github/hub.git
cd hub
make install prefix=/usr/local
```
- Add it to `PATH`: `ln -s /usr/local/hub-<version>/bin/hub /usr/local/bin/hub`
- Aliasing is done already in the `bashrc` and `zshrc` files
- Install [shell tab completion scripts](https://github.com/github/hub#shell-tab-completion).

## Solarized for Xfce terminal

See [here](https://github.com/sgerrand/xfce4-terminal-colors-solarized); simply paste the file from
there into `~/.config/xfce4/terminal/terminalrc`.

Guake has several variants of the Solarized scheme built in, as has Gnome Terminal.

## OhMyRepl.jl

For Julia; simply call `Pkg.add("OhMyRepl")` in the Julia shell.
