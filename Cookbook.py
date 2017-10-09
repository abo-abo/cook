#!/usr/bin/env python
#* Imports
import pycook as pc

#* Functions
def pip_cook_installed_p():
    res = pc.elisp.shell_command_to_string(
        "pip show pycook || echo 'n'").strip()
    return res != "n"

#* Recipes
def pip_reinstall(recipe):
    res = []
    if pip_cook_installed_p():
        res.append("sudo -H pip uninstall -y pycook")
    res.append("sudo -H pip install .")
    return res

def publish(recipe):
    return ["rm -rf dist/",
            "python setup.py sdist",
            "twine upload dist/*"]

def clean(recipe):
    return ["rm -rf dist",
            "rm -rf pycook.egg-info"]

#* Script
if __name__ == '__main__':
    pc.main()
