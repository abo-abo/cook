#* Imports
import re
import os
import shlex
import pycook.elisp as el
from pycook.elisp import sc, lf
os.environ["TERM"] = "linux"

#* Functions
def install_package(package):
    if isinstance(package, list):
        for p in package:
            install_package(p)
    elif el.file_exists_p("/usr/bin/dpkg"):
        res = sc("dpkg --get-selections '{package}'")
        if res == "" or re.search("deinstall$", res):
            el.bash(lf("sudo apt-get install -y {package}"))
        else:
            print(lf("{package}: OK"))
    else:
        res = sc("yum list installed '{package}' &2>1 || true")
        if "Error: No matching Packages to list" in res:
            el.bash(lf("yum update -y && yum upgrade -y && yum install -y '{package}'"))
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
    if el.file_directory_p(to) and not el.file_directory_p(fr):
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
        cmd = lf("ln -s {fr_abbr} {to_full}")
        if not os.access(to_full, os.W_OK):
            cmd = "sudo " + cmd
        el.sc(cmd)

def file_equal(f1, f2):
    def md5sum(f):
        return el.sc("md5sum " + shlex.quote(f)).split(" ")[0]
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
        if os.access(el.expand_file_name(fname), os.W_OK):
            sc("chmod {permissions} {fname}")
        else:
            el.bash(lf("sudo chmod {permissions} {fname}"))

def barf(fname, text):
    if el.file_exists_p(fname) and text == el.slurp(fname):
        print(lf("{fname}: OK"))
    else:
        quoted_text = shlex.quote(text)
        el.bash(lf("echo {quoted_text} | sudo tee {fname} > /dev/null"))

def make(target, cmds, deps=()):
    if (el.file_exists_p(target) and \
        all([os.path.getctime(target) > os.path.getctime(dep) for dep in deps])):
        print(lf("{target}: OK"))
    else:
        if isinstance(cmds, str):
            cmds = [cmds]
        elif callable(cmds):
            cmds()
            return
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

def render_patch(patch_lines, before):
    regex = "^\\+" if before else "^\\-"
    return "\n".join([line[1:] for line in patch_lines if not re.match(regex, line)])

def patch(fname, patches):
    """Patch FNAME applying PATCHES.
    Each PATCH in PATCHES is in diff -u format.
    Each line in PATCH starts with [+- ].
    [ ] lines are the optional start and end context.
    [-] lines are expected to exist in the file and will be removed.
    [+] lines will be added to the file after [-] lines are removed.

    PATCH lines should be in contiguous order: optional start context,
    [-] lines, [+] lines, optional end context.

    If PATCH was already applied for FNAME, it will be ignored.
    """
    fname = el.expand_file_name(fname)
    fname = os.path.realpath(fname)
    if el.file_exists_p(fname):
        txt = el.slurp(fname)
    else:
        assert not any([
            re.search("^\\-", patch, flags=re.MULTILINE)
            for patch in patches])
        el.sc("touch {fname}")
        txt = ""
    no_change = True
    for ptch in patches:
        patch_lines = el.delete("", ptch.splitlines())
        chunk_before = render_patch(patch_lines, True)
        chunk_after = render_patch(patch_lines, False)
        if chunk_before == "":
            if chunk_after not in txt:
                no_change = False
                if not (txt == "" or txt[-1] == "\n"):
                    txt += "\n"
                txt += chunk_after + "\n"
        elif chunk_before in txt:
            no_change = False
            txt = txt.replace(chunk_before, chunk_after)
        else:
            # either already patched or fail
            assert chunk_after in txt
    if no_change:
        print(fname + ": OK")
    else:
        el.barf("/tmp/insta.txt", txt)
        cmd = lf("cp /tmp/insta.txt {fname}")
        if not os.access(fname, os.W_OK):
            cmd = "sudo " + cmd
        el.bash(cmd)
