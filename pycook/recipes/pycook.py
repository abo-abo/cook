#* Imports
import os
import pycook.elisp as el

#* Recipes
def update(recipe):
    el.bash("pip3 install --upgrade pycook")
    cook_el = el.emacs_cook_script("cook.el")
    if "INSIDE_EMACS" in os.environ:
        el.sc(el.emacsclient_eval(el.lf('(load-file "{cook_el}")')))

def profile(recipe):
    return "pyinstrument $(which cook) :linux ls_users"
