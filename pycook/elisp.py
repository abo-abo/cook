#* Imports
import codecs
import subprocess
import sys
import os
import re
import collections

#* Globals
sc_hookfn = None

#* SSH
HOST = None
class hostname:
    def __init__(self, host):
        self._old_host = sys.modules["pycook.elisp"].HOST
        sys.modules["pycook.elisp"].HOST = host

    def __enter__(self):
        pass

    def __exit__(self, *args, **kwargs):
        sys.modules["pycook.elisp"].HOST = self._old_host

#* Functional
def position(item, lst, default=None):
    if item in lst:
        return lst.index(item)
    else:
        return default

def set_difference(lst1, lst2):
    s = set(lst2)
    return [x for x in lst1 if x not in s]

def find_if(pred, lst):
    for item in lst:
        if pred(item):
            return item

def position_if(pred, lst):
    for (i, item) in enumerate(lst):
        if pred(item):
            return i

def flatten(seq):
    """Flatten a list of lists into a list."""
    return [item for sublist in seq for item in sublist]

def partition(n, seq):
    return [seq[i:i + n] for i in range(0, len(seq), n)]

def group_by(f, lst):
    res = collections.OrderedDict()
    for x in lst:
        fx = f(x)
        if fx in res:
            res[fx].append(x)
        else:
            res[fx] = [x]
    return res

def delete(element, lst):
    return [x for x in lst if x != element]

def delete_dups(lst):
    seen = set()
    seen_add = seen.add
    return [x for x in lst if not (x in seen or seen_add(x))]

#* Sys
def top_level():
    f = sys._getframe()
    while f.f_back:
        f = f.f_back
    return f

def crash():
    tf = top_level()
    f = sys._getframe().f_back
    tf.f_globals["lnames"] = f.f_locals.keys()
    for (k, v) in f.f_locals.items():
        tf.f_globals[k] = v
    raise RuntimeError("locals stored to globals")

#* OS
def user_login_name():
    import getpass
    return getpass.getuser()

def emacsclient_eval(expr):
    e = re.sub('"', "\\\"", expr)
    return lf('emacsclient -e "{e}"')

def emacs_cook_script(fname):
    d_lib = locate_dominating_file(__file__, "lib")
    if d_lib:
        return expand_file_name("../cook/" + fname, d_lib)
    else:
        # sys.path has '', handle loading this package from git
        d_etc = locate_dominating_file(__file__, "etc")
        if fname == "cook.el":
            return expand_file_name("../" + fname, d_etc)
        else:
            return expand_file_name(fname, d_etc)

def emacs_batch_eval(expr):
    e = re.sub('"', "\\\"", expr)
    script_el = emacs_cook_script("scripts.el")
    return lf('emacs -batch -l {script_el} --eval "{e}"')

def eeval(s):
    return shell_command_to_string(emacsclient_eval(s))

def beval(s, init_file=None):
    s = re.sub('"', "\\\"", s)
    if init_file:
        init = "-l "+ init_file
    else:
        init = ""
    return shell_command_to_string(lf('emacs -batch {init} --eval "(print {s})"'))

#* Files
def default_directory():
    return os.getcwd()

def locate_dominating_file(f, n):
    if file_directory_p(f):
        d = f
    else:
        d = file_name_directory(expand_file_name(f))
    while d != "/":
        nd = os.path.join(d, n)
        if file_exists_p(nd):
            return nd
        d = file_name_directory(d)

def make_directory(d):
    """Work around Python2/3 `os.makedirs' incompat."""
    if not os.path.exists(d):
        os.makedirs(d)

def expand_file_name(f, directory=None):
    if HOST:
        if ":" in f:
            return f
        else:
            return HOST + ":" + f
    if not directory:
        directory = os.getcwd()
    else:
        directory = os.path.expanduser(directory)
    if re.match("^~", f):
        return os.path.expanduser(f)
    elif re.match("\\.\\./", f):
        return os.path.realpath(os.path.join(directory, f))
    else:
        return os.path.join(directory, f)

def file_name_sans_extension(f):
    return os.path.splitext(f)[0]

def file_name_directory(f):
    return os.path.dirname(f)

def file_name_nondirectory(f):
    return os.path.basename(f)

def parse_fname(fname):
    if not isinstance(fname, str):
        return fname
    elif fname[0] == ".":
        return (None, os.path.realpath(expand_file_name(fname)))
    elif ":" in fname:
        return fname.split(":")
    elif HOST is not None:
        return (HOST, fname)
    else:
        return (None, os.path.realpath(expand_file_name(fname)))

def file_exists_p(f):
    (host, fname) = parse_fname(f)
    if host is not None:
        with hostname(host):
            res = sc(
                "stat {fname} 2>/dev/null || echo Fail",
                desc=(host, "stat " + fname))
            return res != "Fail"
    else:
        return os.path.exists(expand_file_name(fname))

def file_newer_than_file_p(f1, f2):
    return os.path.getctime(f1) > os.path.getctime(f2)

def file_directory_p(f):
    return os.path.isdir(f)

def abbreviate_file_name(f, d):
    if not d[-1] == "/":
        d = d + "/"
    m = re.match(d, f)
    if m:
        return f[m.end():]
    else:
        m = re.match(f, d)
        if m:
            return ".".join(["../"]* (d[m.end():].count("/") - 1))

def directory_files(d, full=False, match=False):
    fs = os.listdir(d)
    if match:
        fs = [f for f in fs if re.search(match, f) is not None]
    if full:
        fs = [expand_file_name(f, d) for f in fs]
    return fs

def delete_file(f):
    return os.remove(f)

#* File read/write
def slurp(f):
    (host, fname) = parse_fname(f)
    if host:
        with hostname(host):
            return sc("cat {fname}")
    else:
        try:
            with open(expand_file_name(f), 'r') as fh:
                return fh.read()
        except UnicodeDecodeError:
            with codecs.open(
                    expand_file_name(f), 'r',
                    encoding="utf-8",
                    errors="ignore") as fh:
                return fh.read()

def slurp_lines(f):
    return slurp(f).splitlines()

def barf(f, s):
    f = os.path.expanduser(f)
    with open(f, 'w') as fh:
        fh.write(s)

#* Shell
def shell_command_to_string(cmd, **kwargs):
    if HOST:
        cmds = ["ssh", HOST, cmd]
    else:
        cmds = ["bash", "-c", cmd]
    out = subprocess.check_output(cmds, **kwargs).strip()
    if isinstance(out, str):
        return out
    else:
        return out.decode()

def sc(cmd, **kwargs):
    fcmd = lf(cmd, 2)
    if "desc" in kwargs:
        desc = kwargs["desc"]
        del kwargs["desc"]
    else:
        desc = None
    if sc_hookfn:
        sc_hookfn(fcmd, desc=desc)
    return shell_command_to_string(fcmd, **kwargs)

def shell_command_to_list(cmd, **kwargs):
    cmd_output = shell_command_to_string(cmd, **kwargs)
    return [s for s in cmd_output.split("\n") if s]

def sc_l(cmd, **kwargs):
    fcmd = lf(cmd, 2)
    if sc_hookfn:
        sc_hookfn(fcmd)
    return shell_command_to_list(fcmd, **kwargs)

def scb(cmd):
    return bash(lf(cmd, 2), capture=True)

def bash(cmd, echo=False, capture=False, **kwargs):
    if isinstance(cmd, list):
        cmd = "\n".join(cmd)
    if echo:
        sep = "-"*80
        print(sep)
        print("Run: \n" + cmd)
        print(sep)
    sys.stdout.flush()

    if HOST:
        cmds = ["ssh", HOST, cmd]
    else:
        cmds = ["/bin/bash", "-e", "-c", cmd]

    if "desc" in kwargs:
        desc = kwargs["desc"]
        del kwargs["desc"]
    else:
        desc = (HOST, cmd)

    if sc_hookfn:
        sc_hookfn(cmd, desc=desc)

    if capture:
        p = subprocess.Popen(cmds, stdout=subprocess.PIPE, stderr=subprocess.PIPE, **kwargs)
        out = ""
        while True:
            part = p.stdout.read().decode()
            if part == "" and p.poll() is not None:
                break
            out += part
            if echo:
                print(part, end="")
        err = ""
        while True:
            part = p.stderr.read().decode()
            if part == "" and p.poll() is not None:
                break
            err += part
            if echo:
                print(part, end="")
        if p.returncode == 0:
            return err + out
        else:
            raise subprocess.CalledProcessError(p.returncode, cmd)
    else:
        p = subprocess.Popen(cmds, **kwargs)
        return_code = p.wait()
        if return_code == 0:
            return 0
        else:
            sys.stdout.flush()
            sys.stderr.flush()
            raise subprocess.CalledProcessError(return_code, cmd)

#* String
def lf(string, lvl=1):
    fr = sys._getframe()
    for _ in range(lvl):
        fr = fr.f_back
    vars_dict = fr.f_globals.copy()
    vars_dict.update(fr.f_locals)
    return string.format(**vars_dict)

#* Regex
def re_filter(regex, seq):
    return list(filter(lambda s: re.search(regex, s), seq))

def re_seq(regex, s):
    return re.findall(regex, s)

def replace_regexp_in_string(regexp, rep, string):
    return re.sub(re.compile(regexp, re.MULTILINE), rep, string)

#* Time
def timestamp():
    from datetime import datetime
    t = datetime.now()
    year = t.year
    month = t.month
    day = t.day
    hour = t.hour
    minute = t.minute
    dow = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"][t.weekday()]
    return lf("<{year}-{month:02d}-{day:02d} {dow} {hour:02d}:{minute:02d}>")
