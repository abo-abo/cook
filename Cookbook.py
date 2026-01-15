#* Imports
from pycook.recipes.pip import clean, build, reinstall, publish
from pycook.recipes.emacs import byte_compile as emacs_byte_compile, checkdoc

#* Recipes
def lint(recipe):
    return ["PYLINTHOME=/tmp/ pylint pycook/"]


def test(recipe, path="pycook/test", flags=""):
    return f"PYTHONPATH=$(pwd) pytest {path} {flags}"

def typecheck_pycook(recipe):
    return ["dmypy run pycook"]

_ = (clean, build, reinstall, publish, emacs_byte_compile, checkdoc)
