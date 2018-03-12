#* Imports
import sys
import shutil
import pycook.elisp as el
sc = el.sc

#* Functions
def get_python():
    return (shutil.which("python3") or
            shutil.which("python"))

def get_pip():
    if sys.version_info.major == 3 and shutil.which("pip3"):
        return "pip3"
    else:
        return "pip"

def package_installed_p(package):
    try:
        s = sc(get_pip() + " show " + package)
        return s != ""
    except:
        return False

def uninstall(package):
    return "sudo -H " + get_pip() + " uninstall -y " + package

def install(package):
    return "sudo -H " + get_pip() + " install " + package

#* Recipes
def reinstall(recipe):
    dd = el.default_directory()
    git1 = el.locate_dominating_file(dd, ".git")
    git2 = el.file_name_directory(git1)
    package = el.file_name_nondirectory(git2)
    if package_installed_p(package):
        res = [uninstall(package)]
    else:
        res = []
    return res + [
        "cd " + git2,
        install(".")]

def sdist(recipe):
    return [
        "rm -rf dist/",
        "python setup.py sdist"]

def clean(recipe):
    return [
        "rm -rf dist",
        "rm -rf *.egg-info"]
