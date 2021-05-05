#* Imports
import sys
import shutil
import pycook.elisp as el
sc = el.sc
lf = el.lf

#* Constants
sudo_cmd = "sudo -H " if el.file_exists_p("/usr/bin/sudo") else "su -c"

#* Functions
def sudo(cmd):
    return sudo_cmd + cmd

def get_pip():
    is_sudo = shutil.which("cook").find("/usr/local/bin") != -1
    if sys.version_info.major == 3:
        pip = "pip3"
    else:
        pip = "pip"
    if is_sudo:
        return sudo(pip)
    else:
        return pip

def package_installed_p(package):
    try:
        s = sc(get_pip() + " show " + package)
        return s != ""
    except:
        return False

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
        return [lf("{pip} install ") + " ".join(to_install)]
    else:
        return []

def reinstall(recipe, user_input=True):
    dd = el.default_directory()
    git1 = el.locate_dominating_file(dd, ".git")
    git2 = el.file_name_directory(git1)
    pip = get_pip()
    return [
        "cd " + git2,
        lf("{pip} install --upgrade .")]

def sdist(recipe):
    return [
        "rm -rf dist/",
        "python3 setup.py sdist"]

def clean(recipe):
    return [
        "rm -rf dist",
        "rm -rf *.egg-info"]

def publish(recipe):
    return sdist(recipe) + ["twine upload dist/*"] + clean(recipe)
