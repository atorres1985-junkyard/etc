#! /usr/bin/env bash

# A terminal-based menu selector for programs on PATH

nicer () {
    compgen -c | sort -u | fzf | xargs setsid -f
}

uglier (){
    echo -n "$PATH" \
        | xargs -d: \
                -I{} \
                -r -- \
                find -L {} \
                -maxdepth 1 -mindepth 1 \
                -type f \
                -executable \
                -printf '%P\n' 2>/dev/null \
        | grep -v '\-wrapped$' \
        | sort -u \
        | fzf \
        | xargs setsid -f
}

uglier
