#* Imports
import pycook.elisp as el
lf = el.lf

#* Recipes
def install(recipe, *packages):
    packages_str = " ".join(packages)
    return el.emacs_batch_eval(lf("(cook-install-deps '({packages_str}))"))

def compile(recipe, *fnames):
    fnames_str = " ".join(['"' + fname + '"' for fname in fnames])
    return el.emacs_batch_eval(lf("(cook-byte-compile {fnames_str})"))
