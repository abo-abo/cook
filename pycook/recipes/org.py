#* Imports
import sys
import os
import re
import pycook.elisp as el
lf = el.lf
sc = el.sc

#* Functions
def emacs_eval(expr):
    e = re.sub('"', "\\\"", expr)
    return lf('emacs -batch -l /usr/local/cook/scripts.el --eval "{e}"')

def export_pdf(recipe, forg):
    fpdf = re.sub("org$", "pdf", forg)
    return [
        emacs_eval(lf('(org-to-pdf "{forg}")')),
        lf("nohup 1>/dev/null 2>/dev/null evince {fpdf} &"),
        el.emacsclient_eval('(with-current-buffer (window-buffer (selected-window)) (bury-buffer))')
    ]

#* Recipes
def clean(recipe):
    return ["rm -rf *.pdf *.tex _minted*"]
