# About

This is a unified repo for some of my configuration files.  A main idea is to consistenly use

- [Solarized](http://ethanschoonover.com/solarized) as a color scheme, mostly in its light variant
- [DejaVu Sans Mono](https://dejavu-fonts.github.io/) as a font, since looks good and has the best
  Unicode support of monospaced fonts, AFAIK (at least when it comes to strange operators in
  programming languages...)
- [JuliaMone](https://juliamono.netlify.app/) in Emacs (I’d fully switch to it, but there currently
  does not exist a Powerline patched variant).  It has been designed specifically for programming in
  Julia, but that just means that it is a well-designed programming font developed for Unicode-heavy
  languages.
  
It mostly stores my settings for zsh with OhMyZsh and Powerlevel9k and my Emacs file. 


# Installations

## Fonts

- JuliaMone from [here](https://cormullion.github.io/pages/2020-07-26-JuliaMono/#linux_-_on_the_command_line)
- DejaVu Sans Mono from [here](https://dejavu-fonts.github.io/)
- For terminals: the Powerline patched variant from [here](https://github.com/powerline/fonts) (I do
  that manually, but there's a script, too).
- Add _Awesome Terminal Fonts_ for additional fallback symbols: clone [the
  repo](https://github.com/gabrielelana/awesome-terminal-fonts) and run `install.sh`

## Github CLI

Get from [here](https://github.com/cli/cli/blob/trunk/docs/install_linux.md)

## ZSH

See [here](https://github.com/robbyrussell/oh-my-zsh/wiki/Installing-ZSH).  To make it the default
shell: `chsh -s $(which zsh)`.

Now, link the file `zshrc` from here to `~/.zshrc`.

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

To get conda completion, add files to `~/.oh-my-zsh/custom/plugins/zsh-completions/src`:

- Conda: https://raw.githubusercontent.com/esc/conda-zsh-completion/master/_conda
- Hub: https://raw.githubusercontent.com/github/hub/master/etc/hub.zsh_completion

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

## Other tools

- [x] [exa](https://github.com/ogham/exa)
- [ ] [fzf](https://github.com/junegunn/fzf), [fzf-zsh](https://github.com/Wyntau/fzf-zsh)
- [x] [diff-so-fancy](https://github.com/so-fancy/diff-so-fancy)
- [x] [bat](https://github.com/sharkdp/bat)

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

(I’m not currently using this, but keep it for future reference.)

- [Install xmonad using stack](https://xmonadhaskell.wordpress.com/2018/10/22/xmonad-ubuntu-18-04-install/)
- Install `dmenu`: `sudo apt-get install suckless-tools`
- [Install `xmobar`](https://xmonadhaskell.wordpress.com/2018/10/22/xmobar-ubuntu-18-04-install/): `stack install --flag xmobar:all_all_extensions xmobar`

[This blog](https://xmonadhaskell.wordpress.com/) and [this introduction](https://beginners-guide-to-xmonad.readthedocs.io/intro.html) is quite useful in general.


# Borg

The ./borg directory contains my
[borgmatic](https://torsion.org/borgmatic/docs/how-to/set-up-backups/) configuration for backups on
my NAS etc.  It is set up in repokey mode.

Steps to reproduce:

- Install Borg locally and on the server.
- (In case you use a NAS, create a user for backups and make sure he has a home directory.)
- Add an SSH key to the server.  Make sure that the `.ssh` directory has the right restricted
  permissions (debug by running `sshd -d -p 1234` on the server and connecting to that port) and
  access is working.
- The passphrase is stored locally in a file which needs to be set up appropriately.
- Maybe the remote borg does not run under that name, so the `remote_path` is explicitely set.
- (Link the keys directory (`storage.borg_keys_directory`) to some place on which you back up the
  key file, if you use that.)
- To run every hour, add the following line with `crontab -e`:

  ```
  0 * * * * /usr/bin/borgmatic --config $HOME/.config/borgmatic/config.yaml --syslog-verbosity 1
  ```
