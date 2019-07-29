#* Imports
import re
import pycook.elisp as el
lf = el.lf

#* Functions
def export_pdf(recipe, fname):
    fpdf = re.sub("org$", "pdf", fname)
    return [
        el.emacs_batch_eval(lf('(org-to-pdf "{fname}")')),
        lf("nohup 1>/dev/null 2>/dev/null evince {fpdf} &"),
        el.emacsclient_eval('(with-current-buffer (window-buffer (selected-window)) (bury-buffer))')
    ]

#* Recipes
def clean(recipe):
    return ["rm -rf *.pdf *.tex _minted*"]
