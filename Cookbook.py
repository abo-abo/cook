#* Imports
import pycook.recipes.pip as pip
from pycook.recipes.pip import clean, sdist, reinstall

#* Recipes
def publish(recipe):
    return [
        "rm -rf dist/",
        "python3 setup.py sdist",
        "twine upload dist/*"
    ]
