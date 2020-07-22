#* Imports
from pycook.recipes.pip import clean, sdist, reinstall, publish
from pycook.recipes.emacs import byte_compile as emacs_byte_compile, checkdoc

#* Recipes
def lint(recipe):
    return ["pylint pycook/"]
