#* Imports
import os
import re
import shlex
import pycook.elisp as el
lf = el.lf

#* Functions
def export_pdf(recipe, fname):
    fpdf = re.sub("org$", "pdf", fname)
    return [
        el.emacs_batch_eval(lf('(cs-org-to-pdf "{fname}")')),
        lf("nohup 1>/dev/null 2>/dev/null evince {fpdf} &"),
        el.emacsclient_eval('(with-current-buffer (window-buffer (selected-window)) (bury-buffer))')
    ]

def export_html(recipe, fname):
    fname = el.expand_file_name(fname)
    fhtml = re.sub("org$", "html", fname)
    el.sc(el.emacs_batch_eval(lf('(cs-org-to-html "{fname}")')))
    if "INSIDE_EMACS" not in os.environ:
        return ["firefox " + shlex.quote(fhtml)]

#* Recipes
def clean(recipe):
    return ["rm -rf *.pdf *.tex _minted*"]
