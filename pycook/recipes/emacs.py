#* Imports
import pycook.elisp as el
lf = el.lf

#* Recipes
def install(recipe, *packages):
    pstr = " ".join(packages)
    return el.emacs_batch_eval(lf("(cook-install-deps '({pstr}))"))
