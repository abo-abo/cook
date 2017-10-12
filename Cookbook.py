#* Imports
from pycook.elisp import sc

#* Functions
def pip3_package_installed_p(package):
    s = sc("pip3 show {package}")
    return s != ""

#* Recipes
def pip_reinstall(recipe):
    res = []
    if pip3_package_installed_p("pycook"):
        res.append("sudo -H pip3 uninstall -y pycook")
    res.append("sudo -H pip3 install .")
    return res

def publish(recipe):
    return ["rm -rf dist/",
            "python setup.py sdist",
            "twine upload dist/*"]

def clean(recipe):
    return ["rm -rf dist",
            "rm -rf pycook.egg-info"]
