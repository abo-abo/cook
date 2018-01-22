_cook_py()
{
  COMPREPLY=( $(_cook_complete "${COMP_WORDS[@]}" "${COMP_CWORD}") )
}
complete -F _cook_py cook
