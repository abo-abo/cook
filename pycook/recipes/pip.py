#* Imports
import sys
import shutil
import pycook.elisp as el
sc = el.sc
lf = el.lf

#* Constants
sudo_cmd = "sudo -H " if el.file_exists_p("/usr/bin/sudo") else "su -c"

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

def sudo(cmd):
    return sudo_cmd + cmd

def uninstall(package):
    return sudo(get_pip() + " uninstall -y " + package)

def reinstall_current(package, pip):
    res = [sudo(lf("{pip} uninstall -y {package}"))] if package_installed_p(package, pip) else []
    return res + [sudo(lf("{pip} install ."))]

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
        return [sudo(lf("{pip} install ") + " ".join(to_install))]
    else:
        return []

def reinstall(recipe, user_input=True):
    dd = el.default_directory()
    git1 = el.locate_dominating_file(dd, ".git")
    git2 = el.file_name_directory(git1)
    pip = get_pip()
    return [
        "cd " + git2,
        sudo(lf("{pip} install --upgrade ."))]

def sdist(recipe):
    return [
        "rm -rf dist/",
        "python setup.py sdist"]

def clean(recipe):
    return [
        "rm -rf dist",
        "rm -rf *.egg-info"]
