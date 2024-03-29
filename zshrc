export TERM="xterm-256color"

# If you come from bash you might have to change your $PATH.
export PATH=/usr/local/texlive/2019/bin/x86_64-linux:~/miniconda3/bin:~/anaconda3/bin:~/.local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH=~/.oh-my-zsh

# Font stuff
POWERLEVEL9K_MODE='awesome-fontconfig'
ZSH_THEME="powerlevel9k/powerlevel9k"
source ~/.fonts/*.sh


custom_python() {
  # Depending on the conda version, either might be set. This
  # variant works even if both are set.
  _path=$CONDA_ENV_PATH$CONDA_PREFIX
  if ! [ -z "$_path" ]; then
    _pyver=$(python -c "import sys; print('{0[0]}.{0[1]}'.format(sys.version_info))")
    echo "\ue63c $_pyver ($(basename $_path))"
  fi
}
POWERLEVEL9K_CUSTOM_PYTHON="custom_python"
POWERLEVEL9K_CUSTOM_PYTHON_BACKGROUND="blue"
POWERLEVEL9K_CUSTOM_PYTHON_FOREGROUND="black"

# POWERLEVEL9K_CONTEXT_TEMPLATE="%n" # default: "%n@%m"
DEFAULT_USER="$USER"

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
POWERLEVEL9K_PROMPT_ON_NEWLINE=true
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(context dir custom_python vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status history background_jobs)
POWERLINE9k_COLOR_SCHEME='light'
POWERLEVEL9K_MULTILINE_LAST_PROMPT_PREFIX='\u2570❯ '
POWERLEVEL9K_SHORTEN_STRATEGY="truncate_middle"


# POWERLEVEL9K_VCS_STAGED_ICON='\u00b1'
# POWERLEVEL9K_VCS_UNTRACKED_ICON='\u25CF'
# POWERLEVEL9K_VCS_UNSTAGED_ICON='\u00b1'
# POWERLEVEL9K_VCS_INCOMING_CHANGES_ICON='\u2193'
# POWERLEVEL9K_VCS_OUTGOING_CHANGES_ICON='\u2191'

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git gh gitignore python colored-man-pages zsh-completions ssh-agent)

source $ZSH/oh-my-zsh.sh

# User configuration

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# ssh
export SSH_KEY_PATH="~/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases

export EDITOR=vim
#export EMACS_SERVER_RUNNING=0
#export SVN_EDITOR=emacs

fpath=(~/.zsh/completions $fpath) 
autoload -U compinit && compinit
autoload -U +X bashcompinit && bashcompinit

#eval "$(stack --bash-completion-script stack)"



