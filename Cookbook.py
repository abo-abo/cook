#* Imports
from pycook.recipes.pip import clean, sdist, reinstall
from pycook.recipes.emacs import byte_compile as emacs_byte_compile, checkdoc

#* Recipes
def publish(recipe):
    return sdist(recipe) + ["twine upload dist/*"] + clean(recipe)

def lint(recipe):
    return ["pylint pycook/"]
