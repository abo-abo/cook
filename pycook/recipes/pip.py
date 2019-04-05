#* Imports
import sys
import shutil
import pycook.elisp as el
sc = el.sc
lf = el.lf

#* Functions
def get_python():
    return (shutil.which("python3") or
            shutil.which("python"))

def get_pip():
    if sys.version_info.major == 3 and shutil.which("pip3"):
        return "pip3"
    else:
        return "pip"

def package_installed_p(package, pip = None):
    try:
        s = sc(pip or get_pip() + " show " + package)
        return s != ""
    except:
        return False

def uninstall(package):
    return "sudo -H " + get_pip() + " uninstall -y " + package

def reinstall_current(package, pip):
    res = [lf("sudo -H {pip} uninstall -y {package}")] if package_installed_p(package, pip) else []
    return res + [lf("sudo -H {pip} install .")]

#* Recipes
def install(recipe, *packages):
    pip = get_pip()
    installed = el.sc_l("{pip} freeze")
    to_install = []
    for p in packages:
        p_v = next((i for i in installed if p + "==" in i), None)
        if p_v:
            print(p_v + ": OK")
        else:
            to_install.append(p)
    if to_install:
        return [lf("sudo -H {pip} install ") + " ".join(to_install)]
    else:
        return []

def reinstall(recipe):
    dd = el.default_directory()
    git1 = el.locate_dominating_file(dd, ".git")
    git2 = el.file_name_directory(git1)
    package_desc = el.slurp(el.expand_file_name("setup.py", git2))
    package = el.re_seq("name='(.*)'", package_desc)[0]
    if package_installed_p(package):
        res = [uninstall(package)]
    else:
        res = []
    res += ["cd " + git2] + install(recipe, ".")
    return res

def sdist(recipe):
    return [
        "rm -rf dist/",
        "python setup.py sdist"]

def clean(recipe):
    return [
        "rm -rf dist",
        "rm -rf *.egg-info"]
