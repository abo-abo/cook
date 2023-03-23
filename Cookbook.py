#* Imports
from pycook.recipes.pip import clean, build, reinstall, publish
from pycook.recipes.emacs import byte_compile as emacs_byte_compile, checkdoc

#* Recipes
def lint(recipe):
    return ["PYLINTHOME=/tmp/ pylint pycook/"]

def test(recipe):
    return ["PYTHONPATH=$(pwd) pytest pycook/test/"]

_ = (clean, build, reinstall, publish, emacs_byte_compile, checkdoc)
