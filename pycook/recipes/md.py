#* Imports
import re
import shlex
import pycook.elisp as el
lf = el.lf

#* Functions
def export_org(recipe, fname):
    forg = re.sub("md$", "org", fname)
    return [
        (
            "pandoc -f markdown -t org --columns=100 -o " +
            shlex.quote(forg) + " " + shlex.quote(fname)),
        el.emacs_batch_eval(lf('(cs-md-cleanup "{fname}")'))]
