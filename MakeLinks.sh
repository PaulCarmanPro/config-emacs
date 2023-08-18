#! /bin/bash
#
# MakeLinks.sh [-f] [-v] DESTINATION-DIRECTORY

#shellcheck disable=SC2155 # declare and assign separately
#shellcheck disable=SC2086 # doublequote to prevent splitting

set -f # no filename expansion (globbing)

declare dErr # many sources lacking destination dumps many identical errors
Err() {
   local zErr="$(printf '!!![%s]!!! %s\n' "${BASH_SOURCE[0]}" "$@")"
   if [[ "$zErr" != "$dErr" ]]; then
      printf '%s' "$zErr"
      dErr="$zErr"
   fi
}

CreateDirectory() { # aForce aVerbose zDest=Directory
   # the contents of an existing directory are left alone
   # allows user to have/keep extra files not in repository
   [[ -d "$zDest" ]] && return # directory already exists
   if [[ -e "$zDest" ]]; then
      if cmp "$zDest" "$zDest~" 2>/dev/null; then
         rm "$zDest" # already backed up
      elif [[ "$aForce" ]]; then
         rm -rf$aVerbose "$zDest"
      else
         mv -b$aVerbose "$zDest" "$zDest~"
      fi
   fi
   mkdir -p$aVerbose "$zDest"
}

CreateLink() { # aForce aVerbose zSource=File zDest
   # create a symbolic link to zSource at zDest
   # unless zDest is identical to zSource, then the file will remain
   # create backup of original if aFlags doesnot contain f
   cmp "$zDest" "$zSource" 2>/dev/null && return # already identical
   if [[ -e "$zDest" ]]; then
      if cmp "$zDest" "$zDest~" 2>/dev/null; then # already backed up
         rm -r "$zDest"
      elif [[ "$aForce" ]]; then # flags force overwrite
         rm -r$aVerbose "$zDest"
      else
         mv -b$aVerbose "$zDest" "$zDest~" # create backup
      fi
   fi
   ln -${aVerbose}sT "$zSource" "$zDest"
}

CreateLinks() { # aForce aVerbose aDest=Directory < SOURCES_NULL_TERMINATED
   local z zDest zSource
   while IFS= read -r -d $'\0' z; do
      zDest="$aDest${z:+/}$z"
      zSource="$aSource${z:+/}/$z"
      if [[ -d "$zSource" ]]; then
         CreateDirectory
      else
         CreateLink
      fi
   done
}

declare aSource
declare a # current loop argument
unset aDest # unset is the flag to reassign aDest 
declare zDest # back-checked for over-rename error
while 0 != "$#"; do
   a="$1"
   shift # shift now to allow continue (and extra argument consumption)
   if [[ '-d' = "$a" ]]; then # assign destination directory next iteration
      unset aDest zRename
   elif [[ '-r' = "$a" ]]; then # assign destination name next iteration
      zRename='-r'
      unset aDest
   elif [[ -z "${aDest+x}" ]]; then # reassign destination
      declare aDest="$a"
      aDest="$(realpath -e "$aDest")" \
         || Err "Could not resolve destination path aDest."
   else # recursively create links to files found in source
      if ! aSource="$(realpath -e "$a")"; then
         Err "Could not resolve source path $aSource."
         continue
      elif [[ "$aDest" = "$aSource" ]]; then
         Err "Destination identical to source aSource."
         continue
      elif [[ '+r' = "$zRename" ]]; then
         Err "Cannot rename (-r) multiple sources to the same destination $zDest."
         continue
      else # loop through found files
         CreateLinks  < <(find "$aSource" -printf '%P\0' | sort -zV)
      fi
   fi
done

