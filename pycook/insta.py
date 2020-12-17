#* Imports
import re
import os
import shlex
import pycook.elisp as el
import pycook.recipes.git as git
from pycook.elisp import sc, lf, bash, parse_fname, scb, hostname
os.environ["TERM"] = "linux"

#* Functions
def package_installed_p_dpkg(package):
    res = sc("dpkg --get-selections '{package}'")
    if res == "" or re.search("deinstall$", res):
        return False
    else:
        return True

def package_installed_p_yum(package):
    res = scb("yum list installed '{package}' || true")
    return "Error: No matching Packages to list" not in res

def install_package(package, url=None):
    if isinstance(package, list):
        res = False
        for p in package:
            res |= install_package(p, url)
        return res
    user = sc("whoami")
    su = "" if user == "root" else "sudo "
    if el.file_exists_p("/usr/bin/dpkg"):
        if package_installed_p_dpkg(package):
            print(lf("{package}: OK"))
            return False
        else:
            if url is None:
                bash(lf("{su}apt-get update && DEBIAN_FRONTEND=noninteractive {su}apt-get install -y {package}"))
            else:
                fname = wget(url)
                bash(lf("{su}dpkg -i {fname}"))
            return True
    else:
        if package_installed_p_yum(package):
            print(lf("{package}: OK"))
            return False
        else:
            if url is None:
                bash(lf("{su}yum update -y && {su}yum upgrade -y && {su}yum install -y '{package}'"))
            else:
                fname = wget(url)
                bash(lf("{su}yum localinstall -y {fname}"))
            return True

def apt_key_add(email, url):
    if grep("apt-key list", email):
        print(email + " key: OK")
    else:
        bash(lf("wget -qO - {url} | apt-key add -"))

def debconf_select(package, var, v_type, v_val):
    bash(lf("echo '{package} {var} {v_type} {v_val}' | debconf-set-selections"))

def wget(url, download_dir="/tmp/"):
    if download_dir[:-1] == "/":
        fname = url.split("/")[-1]
        full_name = el.expand_file_name(fname, download_dir)
    else:
        full_name = download_dir
    if el.file_exists_p(full_name):
        print(lf("{full_name}: OK"))
    else:
        bash(lf("wget '{url}' -O {full_name}"))
        return full_name

def systemctl_start(service):
    if not el.scb("systemctl is-active {service} || true") == "active\n":
        el.bash(lf("systemctl start {service}"))
        return True
    else:
        print(lf("{service}: OK"))
        return False

def systemctl_enabled_services():
    cmd = "systemctl list-unit-files | grep enabled | grep service"
    lines = [re.split("[ \t]+", l)[0] for l in el.sc_l(cmd)]
    return [re.split("[.@]", l)[0] for l in lines]

def systemctl_enable(service):
    if service in systemctl_enabled_services():
        print(lf("{service}: OK"))
        return False
    else:
        el.bash(lf("systemctl enable {service}"))
        return True

def git_clone(url, target_dir, commit=None):
    target_dir = el.expand_file_name(target_dir)
    if el.file_exists_p(target_dir):
        print(lf("{target_dir}: OK"))
    else:
        gdir = el.expand_file_name(target_dir)
        pdir = el.file_name_directory(gdir)
        if not el.file_exists_p(pdir):
            el.make_directory(pdir)
        (_, gdir) = el.parse_fname(gdir)
        sc("git clone --recursive {url} {gdir}")
        if commit:
            sc("cd {gdir} && git reset --hard {commit}")

def symlink_p(fname):
    return " -> " in el.sc("stat {fname}")

def sudo(cmd, fname=None):
    if fname:
        if os.access(fname, os.W_OK):
            return cmd
        elif os.access(os.path.dirname(fname), os.W_OK | os.X_OK):
            return cmd
        else:
            return "sudo " + cmd
    user = sc("whoami")
    su = "" if user == "root" else "sudo "
    return su + cmd

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
        cmd = sudo(lf("ln -s {fr_abbr} {to_full}"), to_full)
        bash(cmd)

def file_equal(f1, f2):
    def md5sum(f):
        return el.sc("md5sum " + shlex.quote(f)).split(" ")[0]
    return md5sum(f1) == md5sum(f2)

def cp_host(fr, to):
    # if el.file_exists_p(to) and file_equal(fr, to):
    #     print(lf("{to}: OK"))
    #     return False
    host = el.HOST
    with el.hostname(None):
        fr = el.expand_file_name(fr)
        el.sc("scp -r '{fr}' '{host}:{to}'")
        return True

def echo(fr_text, to):
    if el.file_exists_p(to) and fr_text == el.slurp(to):
        print(lf("{to}: OK"))
        return False
    elif el.HOST is None:
        el.sc(f"echo '{fr_text}' | " + sudo(f"tee {to}", to))
        return True
    else:
        host = el.HOST
        with el.hostname(None):
            el.sc(
                "echo '{fr_text}' | ssh '{host}' -T 'cat > {to}'",
                desc=(host, "write " + to))
            return True

def cp(fr, to):
    fr = el.expand_file_name(fr)
    to = el.expand_file_name(to)
    if el.file_exists_p(to) and file_equal(fr, to):
        print(lf("{to}: OK"))
        return False
    else:
        el.sc(sudo("cp '{fr}' '{to}'", to))
        return True

def chmod(fname, permissions):
    current = sc("stat -c %a {fname}")
    if current == permissions:
        print(lf("{fname}: OK"))
        return False
    else:
        if el.HOST is not None:
            scb("chmod {permissions} {fname}")
        else:
            cmd = sudo(lf("chmod {permissions} {fname}"), fname)
            bash(cmd)
        return True

def chown(fname, owner):
    current = sc("stat -c %U:%G {fname}")
    if current == owner:
        print(lf("{fname}: OK"))
        return False
    else:
        if el.HOST is not None:
            bash(lf("chown -R {owner} {fname}"))
        else:
            bash(lf("sudo chown -R {owner} {fname}"))
        return True

def barf(fname, text):
    if el.file_exists_p(fname) and text == el.slurp(fname):
        print(lf("{fname}: OK"))
    else:
        quoted_text = shlex.quote(text)
        bash(lf("echo {quoted_text} | sudo tee {fname} > /dev/null"))

def get_change_time(fname):
    fname = el.expand_file_name(fname)
    if el.file_directory_p(fname):
        git_dir = el.expand_file_name(".git", fname)
        if el.file_exists_p(git_dir):
            return float(git.mtime(fname).strftime("%s"))
        else:
            raise RuntimeError("Directory is not a git repo")
    else:
        return os.path.getctime(fname)

def make(target, cmds, deps=()):
    if (el.file_exists_p(target) and \
        all([get_change_time(target) > get_change_time(dep) for dep in deps])):
        print(lf("{target}: OK"))
        return False
    else:
        if isinstance(cmds, str):
            cmds = [cmds]
        elif callable(cmds):
            cmds(target)
            return True
        fcmds = []
        for cmd in cmds:
            cmd1 = re.sub("\\$@", target, cmd)
            cmd2 = re.sub("\\$\\^", " ".join([shlex.quote(dep) for dep in deps]), cmd1)
            cmd3 = re.sub("\\$<", shlex.quote(deps[0]), cmd2) if deps else cmd2
            if el.sc_hookfn:
                el.sc_hookfn(cmd3)
            fcmds.append(cmd3)
        bash(fcmds)
        return True

def curl(link, directory="~/Software"):
    fname = el.expand_file_name(link.split("/")[-1], directory)
    make(fname, "curl " + link + " -o " + shlex.quote(fname))
    return fname

def grep(cmd, regex):
    fcmd = cmd + " 2>/dev/null | grep -q '" + regex + "' && echo Y || echo N"
    return el.sc(fcmd) == "Y"

def render_patch(patch_lines, before):
    regex = "^\\+" if before else "^\\-"
    return "\n".join([line[1:] for line in patch_lines if not re.match(regex, line)])

def parse_patches(patches):
    if isinstance(patches, list):
        return [p if isinstance(p, str) else "\n".join(p) for p in patches]
    ls = el.slurp_lines(patches)
    res = []
    cur = []
    i = 0
    n = len(ls)
    while i < n:
        l = ls[i]
        if re.match("---|\+\+\+|@@", l):
            cur = []
            i += 1
        elif re.match(" ", l):
            cur.append(l)
            i += 1
        elif re.match("[+-]", l):
            cur.append(l)
            i += 1
            while re.match("[+-]", ls[i]) and i < n:
                cur.append(ls[i])
                i += 1
            res.append("\n".join(cur))
            cur = []
        elif l == "":
            i += 1
        else:
            raise RuntimeError("Unexpected", l)
    return res

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
    PATCHES can also be a file name of a "diff -u" output.
    """
    (host, name) = parse_fname(fname)
    patches = parse_patches(patches)
    if el.file_exists_p(fname):
        txt = el.slurp(fname)
    else:
        assert not any([
            re.search("^\\-", patch, flags=re.MULTILINE)
            for patch in patches])
        el.sc("touch {name}")
        txt = ""
    no_change = True
    for ptch in patches:
        patch_lines = el.delete("", ptch.splitlines())
        chunk_before = render_patch(patch_lines, True)
        chunk_after = render_patch(patch_lines, False)
        if chunk_before == "":
            if not re.search("^" + re.escape(chunk_after), txt, re.MULTILINE):
                no_change = False
                if not (txt == "" or txt[-1] == "\n"):
                    txt += "\n"
                txt += chunk_after + "\n"
        elif (re.search("^" + re.escape(chunk_before), txt, re.MULTILINE) and
              not re.search("^" + re.escape(chunk_after), txt, re.MULTILINE)):
            no_change = False
            txt = re.sub("^" + re.escape(chunk_before), chunk_after, txt, flags=re.MULTILINE)
        else:
            # either already patched or fail
            if not chunk_after in txt:
                raise RuntimeError("Patch failed", ptch)
    if no_change:
        print(fname + ": OK")
        return False
    else:
        print(fname + ": Patch")
        with el.hostname(None):
            el.barf("/tmp/insta.txt", txt)
        if host is not None:
            cmd = lf("scp /tmp/insta.txt {host}:{name}")
        else:
            cmd = sudo(lf("cp /tmp/insta.txt {name}"), name)
        with el.hostname(None):
            bash(cmd, desc=(host, "edit " + name))
        return True
