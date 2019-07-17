#* Imports
import re
import os
import shlex
import pycook.elisp as el
sc = el.sc
lf = el.lf
os.environ["TERM"] = "linux"

#* Functions
def install_package(package):
    if type(package) is list:
        [install_package(p) for p in package]
    else:
        res = sc("dpkg --get-selections '{package}'")
        if not len(res) or re.search("deinstall$", res):
            el.bash(lf("sudo apt-get install -y {package}"))
        else:
            print(lf("{package}: OK"))

def git_clone(addr, target, commit=None):
    target = el.expand_file_name(target)
    if el.file_exists_p(target):
        print(lf("{target}: OK"))
    else:
        gdir = el.expand_file_name(target)
        pdir = el.file_name_directory(gdir)
        if not el.file_exists_p(pdir):
            el.make_directory(pdir)
        sc("git clone --recursive {addr} {gdir}")
        if commit:
            sc("cd {pdir} && git reset --hard {commit}")

def symlink_p(fname):
    return " -> " in el.sc("stat {fname}")

def ln(fr, to):
    fr = el.expand_file_name(fr)
    if not el.file_exists_p(fr):
        raise RuntimeError("File doesn't exist", fr)
    to = el.expand_file_name(to)
    if el.file_directory_p(to):
        to_full = el.expand_file_name(el.file_name_nondirectory(fr), to)
    else:
        to_full = to
    if el.file_exists_p(to_full):
        if symlink_p(to_full):
            print(lf("{to_full}: OK"))
        else:
            if file_equal(fr, to_full):
                print(lf("{to_full} exists, contents equal"))
            else:
                print(lf("{to_full} exists, contents NOT equal"))
    else:
        fr_abbr = os.path.relpath(fr, os.path.dirname(to))
        el.sc("ln -s {fr_abbr} {to_full}")

def file_equal(f1, f2):
    def md5sum(f):
        return el.sc("md5sum {f}").split(" ")[0]
    return md5sum(f1) == md5sum(f2)

def cp(fr, to):
    if el.file_exists_p(to) and file_equal(fr, to):
        print(lf("{to}: OK"))
    else:
        el.sc("cp '{fr}' '{to}'")

def chmod(fname, permissions):
    current = sc("stat -c '%a' {fname}")
    if current == permissions:
        print(lf("{fname}: OK"))
    else:
        el.bash(lf("sudo chmod {permissions} {fname}"))

def barf(fname, text):
    if el.file_exists_p(fname) and text == el.slurp(fname):
        print(lf("{fname}: OK"))
    else:
        quoted_text = shlex.quote(text)
        el.bash(lf("echo {quoted_text} | sudo tee {fname} > /dev/null"))

def make(target, cmds, deps=[]):
    if type(cmds) is str:
        cmds = [cmds]
    if (el.file_exists_p(target) and
        all([os.path.getctime(target) > os.path.getctime(dep) for dep in deps])):
        print(lf("{target}: OK"))
    else:
        fcmds = []
        for cmd in cmds:
            cmd1 = re.sub("\\$@", target, cmd)
            cmd2 = re.sub("\\$\\^", " ".join([shlex.quote(dep) for dep in deps]), cmd1)
            cmd3 = re.sub("\\$<", shlex.quote(deps[0]), cmd2) if deps else cmd2
            if el.sc_hookfn:
                el.sc_hookfn(cmd3)
            fcmds.append(cmd3)
        el.bash(fcmds)

def curl(link, directory="~/Software"):
    fname = el.expand_file_name(link.split("/")[-1], directory)
    make(fname, "curl " + link + " -o " + shlex.quote(fname))
    return fname
