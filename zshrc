# prompt
PROMPT='[%n@%m %1~]%# '

# history
HISTFILE=~/.zsh_history
SAVEHIST=10000
HISTSIZE=10000
setopt HIST_FIND_NO_DUPS     # Do not display a line previously found.
setopt HIST_IGNORE_ALL_DUPS  # Remove duplicate commands from history
setopt HIST_IGNORE_SPACE     # Don't save commands starting with space
setopt HIST_REDUCE_BLANKS    # Remove superfluous blanks before recording entry.
setopt HIST_SAVE_NO_DUPS     # Don't write duplicate entries in the history file.
setopt INC_APPEND_HISTORY    # Write immediately, not on exit
setopt SHARE_HISTORY         # Share history between all zsh sessions

# editor
export EDITOR="emacsclient"

# colors
if [[ -z "$INSIDE_EMACS" ]]; then
    export TERM=xterm-256color
fi
if ls --color > /dev/null 2>&1; then
    # GNU ls (Linux)
    alias ls='ls --color=auto'
else
    # BSD ls (macOS)
    alias ls='ls -G'
fi
export CLICOLOR=1

# kbd
bindkey -e  # emacs keybindings

# completion
autoload -Uz compinit
compinit

if [[ "${INSIDE_EMACS%%,*}" = 'ghostel' ]]; then
   source "$EMACS_GHOSTEL_PATH/etc/shell/ghostel.zsh"

    # Open a file in Emacs from the terminal
    e()   { ghostel_cmd find-file-other-window "$@"; }

    # Open dired, defaulting to the current directory
    d() { ghostel_cmd dired "${1:-$PWD}"; }
fi

# aliases
if [[ "$(uname)" == "Darwin" ]]; then
    alias noiseon="(afplay \"$HOME/Sounds/brown-noise.mp3\" > /dev/null 2>&1 &) && echo 'Brown noise started'"
    alias noiseoff="pkill -f 'afplay.*brown-noise' 2>/dev/null && echo 'Brown noise stopped' || echo 'No brown noise playing'"
elif [[ "$(uname)" == "Linux" ]]; then
    alias noiseon="(pw-play \"$HOME/Sounds/brown-noise.mp3\" > /dev/null 2>&1 &) && echo 'Brown noise started'"
    alias noiseoff="pkill -f 'pw-play.*brown-noise' 2>/dev/null && echo 'Brown noise stopped'"
fi
alias ytd="$HOME/dotfiles/scripts/yt-download.sh"

# path
export PATH="$HOME/bin:$HOME/.local/bin:$PATH"
