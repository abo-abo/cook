_cookbook_py()
{
  local cur recipes sname
  cur="${COMP_WORDS[COMP_CWORD]}"
  # need eval echo here because ~/foo/script becomes '~/foo/script'
  sname=$(realpath $(eval echo ${COMP_WORDS[0]}))
  recipes=`${sname} --list`
  COMPREPLY=( $(compgen -W "${recipes}" -- ${cur}) )
}
complete -F _cookbook_py Cookbook.py
