#* Imports
import os
import pycook.elisp as el
import pycook.insta as st

#* Recipes
def update(recipe):
    el.bash("pip3 install --upgrade pycook")
    cook_el = el.emacs_cook_script("cook.el")
    if "INSIDE_EMACS" in os.environ:
        el.sc(el.emacsclient_eval(el.lf('(load-file "{cook_el}")')))

def setup_dotfiles(recipe):
    st.patch("~/.profile", ['+PATH="$HOME/.local/bin:$PATH"'])
    st.patch("~/.bashrc", ["""
+if [ -f ~/.local/cook/bash-completion.sh ]; then
+    . ~/.local/cook/bash-completion.sh
+fi
"""])
    st.patch("~/.bash_profile", ["+. ~/.profile", "+. ~/.bashrc"])

def profile(recipe):
    return "pyinstrument $(which cook) :linux ls_users"
