#* Imports
import pycook.recipes.pip as pip
from pycook.recipes.pip import clean, sdist

#* Recipes
def pip_reinstall(recipe):
    res = []
    if pip.package_installed_p("pycook"):
        res += [pip.uninstall("pycook")]
    res += [pip.install(".")]
    return res

def publish(recipe):
    return [
        "rm -rf dist/",
        "python3 setup.py sdist",
        "twine upload dist/*"
    ]
