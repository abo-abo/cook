#* Imports
import re
import pycook.elisp as el
from shutil import which
from pycook.insta import bash, wget, install_package, make, run

#* Functions
def install_ripgrep(version="13.0.0"):
    is_rg_installed = which("rg")
    if is_rg_installed:
        existing_version = run("rg --version")
        m = re.match("ripgrep ([0-9]+\\.[0-9]+\\.[0-9]+)", existing_version)
        if m is None:
            print("unexpected ripgrep format: {}")
        if m.group(1) == version:
            print(f"rg {version}: OK")
            return
        else:
            bash(f"sudo apt-get remove ripgrep")
    install_package(
        "ripgrep",
        f"https://github.com/BurntSushi/ripgrep/releases/download/{version}/ripgrep_{version}_amd64.deb")

def install_go(version="1.19"):
    existing_exe = which("go")
    if existing_exe:
        existing_version = el.re_find("go([0-9]+\\.[0-9]+)", run(f"{existing_exe} version"))
        print("Found go: ", existing_version)
        if existing_version == version:
            print("Versions match. Exit")
            return
        bash("sudo rm -rf /usr/local/go /usr/local/bin/go")

    tar_file = f"go{version}.linux-amd64.tar.gz"
    tar_file_full = el.expand_file_name(tar_file, "/tmp/")
    url = f"https://golang.org/dl/go{version}.linux-amd64.tar.gz"
    fname = wget(url, "~/Software/")
    make("/usr/local/go", [
        "sudo rm -rf '/usr/local/go'",
        f"cat {fname} | sudo tar -xz -C /usr/local"])
    make("/usr/local/bin/go", [
        "sudo ln -sf /usr/local/go/bin/go $@"])

package_installers = {
    "ripgrep": install_ripgrep,
    "go": install_go
}

def install_package_version(package, version=None):
    if package in package_installers:
        install_fn = package_installers[package]
        if version is None:
            return install_fn()
        else:
            return install_fn(version)
    else:
        print("Falling back to the system package manager...")
        install_package(package)

#* Recipes
def install(recipe, package, version=None):
    if version in ["None", ""]:
        version = None
    install_package_version(package, version)
