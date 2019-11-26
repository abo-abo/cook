#* Imports
from pycook.recipes.pip import clean, sdist, reinstall
from pycook.recipes.emacs import byte_compile as emacs_byte_compile

#* Recipes
def publish(recipe):
    return [
        "rm -rf dist/",
        "python3 setup.py sdist",
        "twine upload dist/*"
    ]

def lint(recipe):
    return ["pylint pycook/"]
