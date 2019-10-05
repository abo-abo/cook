_cook_py()
{
  local IFS=$'\n'
  compopt -o filenames 2>/dev/null
  COMPREPLY=( $(_cook_complete "${COMP_WORDS[@]}" "${COMP_CWORD}") )
}
complete -F _cook_py cook
