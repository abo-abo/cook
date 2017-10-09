_cookbook_py()
{
  local cur recipes sname
  cur="${COMP_WORDS[COMP_CWORD]}"
  if [ "$COMP_CWORD" -lt "2" ]; then
      # need eval echo here because ~/foo/script becomes '~/foo/script'
      sname=$(realpath $(eval echo ${COMP_WORDS[0]}))
      recipes=`${sname} --list`
      COMPREPLY=( $(compgen -W "${recipes}" -- ${cur}) )
  else
    COMPREPLY=()
  fi
}
complete -F _cookbook_py Cookbook.py

_cook_py()
{
  local cur recipes
  cur="${COMP_WORDS[COMP_CWORD]}"
  if [ "$COMP_CWORD" -lt "2" ]; then
      recipes=`cook --list`
      COMPREPLY=( $(compgen -W "${recipes}" -- ${cur}) )
  else
    COMPREPLY=()
  fi
}
complete -F _cook_py cook
