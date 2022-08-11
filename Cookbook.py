#* Imports
from pycook.recipes.pip import clean, sdist, reinstall, publish
from pycook.recipes.emacs import byte_compile as emacs_byte_compile, checkdoc

#* Recipes
def lint(recipe):
    return ["PYLINTHOME=/tmp/ pylint pycook/"]

def test(recipe):
    return ["PYTHONPATH=$(pwd) pytest pycook/test/test_cook.py"]
