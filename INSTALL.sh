#! /bin/bash
#shellcheck disable=SC2155 # declare and assign separately

declare dSource="$(dirname -- "${BASH_SOURCE[0]}")"
declare dName='emacs.d'
"$dSource/MakeLinks.sh" "$HOME/" "$dSource/$dName" 
