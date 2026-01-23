#* Imports
import os
import re
import signal
import subprocess
import sys
from typing import Any, Callable, Optional, Tuple

#* Globals
sc_hookfn: Optional[Callable[[str, Optional[Tuple[Optional[str], str]]], None]] = None

#* SSH
HOST: Optional[str] = None
class hostname:
    def __init__(self, host: Optional[str]) -> None:
        self._old_host = sys.modules["pycook.elisp"].HOST
        sys.modules["pycook.elisp"].HOST = host # type: ignore[attr-defined]

    def __enter__(self) -> None:
        pass

    def __exit__(self, *args: Any, **kwargs: Any) -> None:
        sys.modules["pycook.elisp"].HOST = self._old_host  # type: ignore[attr-defined]

SUDO = ""
class su:
    def __enter__(self) -> None:
        sys.modules["pycook.elisp"].SUDO = "sudo "  # type: ignore[attr-defined]

    def __exit__(self, *args: Any, **kwargs: Any) -> None:
        sys.modules["pycook.elisp"].SUDO = ""  # type: ignore[attr-defined]


#* Functional
def position(item: Any, lst: list[Any], default: Any = None) -> Any:
    if item in lst:
        return lst.index(item)
    else:
        return default

def set_difference(lst1: list[Any], lst2: list[Any]) -> list[Any]:
    s = set(lst2)
    return [x for x in lst1 if x not in s]

def find_if(pred: Callable[[Any], bool], lst: list[Any]) -> Optional[Any]:
    for item in lst:
        if pred(item):
            return item
    return None

def position_if(pred: Callable[[Any], bool], lst: list[Any]) -> Optional[int]:
    for (i, item) in enumerate(lst):
        if pred(item):
            return i
    return None

def flatten(seq: list[list[Any]]) -> list[Any]:
    """Flatten a list of lists into a list."""
    return [item for sublist in seq for item in sublist]

def partition(n: int, seq: list[Any]) -> list[list[Any]]:
    return [seq[i:i + n] for i in range(0, len(seq), n)]

def delete(element: Any, lst: list[Any]) -> list[Any]:
    return [x for x in lst if x != element]

def delete_dups(lst: list[Any]) -> list[Any]:
    seen: set[Any] = set()
    seen_add = seen.add
    return [x for x in lst if not (x in seen or seen_add(x))]

#* Sys
def top_level():
    f = sys._getframe()
    while f.f_back:
        f = f.f_back
    return f

def crash() -> None:
    tf = top_level()
    f = sys._getframe().f_back
    assert f is not None
    tf.f_globals["lnames"] = f.f_locals.keys()
    for (k, v) in f.f_locals.items():
        tf.f_globals[k] = v
    raise RuntimeError("locals stored to globals")

#* OS
def user_login_name() -> str:
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
def default_directory() -> str:
    return os.getcwd()

def locate_dominating_file(f: str, n: str) -> Optional[str]:
    if file_directory_p(f):
        d = f
    else:
        d = file_name_directory(expand_file_name(f))
    while d != "/":
        nd = os.path.join(d, n)
        if file_exists_p(nd):
            return nd
        d = file_name_directory(d)
    return None

def make_directory(d: str) -> None:
    """Work around Python2/3 `os.makedirs' incompat."""
    d = os.path.expanduser(d)
    if not os.path.exists(d):
        os.makedirs(d)

def expand_file_name(f: str, directory: Optional[str] = None) -> str:
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

def file_name_sans_extension(f: str) -> str:
    return os.path.splitext(f)[0]

def file_name_directory(f: str) -> str:
    return os.path.dirname(f)

def file_name_nondirectory(f: str) -> str:
    return os.path.basename(f)

def parse_fname(fname: str) -> tuple[Optional[str], str]:
    if not isinstance(fname, str):
        return fname
    elif fname[0] == ".":
        return (None, os.path.realpath(expand_file_name(fname)))
    # elif ":" in fname:
    #     return fname.split(":")
    elif HOST is not None:
        return (HOST, fname)
    else:
        return (None, os.path.realpath(expand_file_name(fname)))

def file_exists_p(f: str) -> bool:
    (host, fname) = parse_fname(f)
    if host is not None:
        with hostname(host):
            res = sc(
                f"{SUDO}stat {fname} 2>/dev/null || echo Fail",
                desc=(host, "stat " + fname))
            return res != "Fail"
    else:
        return os.path.exists(expand_file_name(fname))

def file_newer_than_file_p(f1: str, f2: str) -> bool:
    return os.path.getmtime(f1) > os.path.getmtime(f2)

def file_directory_p(f: str) -> bool:
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

def directory_files(d: str, full: bool = False, match: str | bool = False) -> list[str]:
    fs = os.listdir(d)
    if match:
        fs = [f for f in fs if re.search(match, f) is not None]  # type: ignore[arg-type]
    if full:
        fs = [expand_file_name(f, d) for f in fs]
    return fs

def delete_file(f: str) -> None:
    os.remove(f)

#* File read/write
def barf(f: str, s: str) -> None:
    f = os.path.expanduser(f)
    with open(f, 'w') as fh:
        fh.write(s)

#* Shell
def shell_command_to_string(cmd: str, **kwargs: Any) -> str:
    if HOST:
        cmds = ["ssh", HOST, cmd]
    else:
        cmds = ["bash", "-c", cmd]
    out = subprocess.check_output(cmds, **kwargs).strip()
    if isinstance(out, str):
        return out
    else:
        return out.decode()

def sc(cmd: str, **kwargs: Any) -> str:
    fcmd = lf(cmd, 2)
    if "desc" in kwargs:
        desc = kwargs["desc"]
        del kwargs["desc"]
    else:
        desc = None
    if sc_hookfn:
        sc_hookfn(fcmd, desc)
    return shell_command_to_string(fcmd, **kwargs)

def shell_command_to_list(cmd, **kwargs):
    cmd_output = shell_command_to_string(cmd, **kwargs)
    return [s for s in cmd_output.split("\n") if s]

def sc_l(cmd: str, **kwargs: Any) -> list[str]:
    fcmd = lf(cmd, 2)
    if sc_hookfn:
        sc_hookfn(fcmd, None)
    return shell_command_to_list(fcmd, **kwargs)

def scb(cmd):
    return bash(lf(cmd, 2), capture=True).strip()

def bash(cmd, echo=False, capture=False, **kwargs):
    if isinstance(cmd, list):
        cmd = "\n".join(cmd)
    if echo:
        sep = "-"*80
        print(sep, file=sys.stderr)
        print("Run: \n" + cmd, file=sys.stderr)
        print(sep, file=sys.stderr)
    sys.stderr.flush()

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
        sc_hookfn(cmd, desc)

    if capture:
        p = subprocess.Popen(cmds, stdout=subprocess.PIPE, stderr=subprocess.PIPE, **kwargs)
        assert p.stdout is not None
        assert p.stderr is not None
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
            print(err + out)
            raise subprocess.CalledProcessError(p.returncode, cmd)
    else:
        def signal_handler(sig, frame):
            pass

        signal.signal(signal.SIGINT, signal_handler)
        p = subprocess.Popen(cmds, **kwargs)
        return_code = p.wait()
        if return_code == 0:
            return 0
        else:
            sys.stdout.flush()
            sys.stderr.flush()
            raise subprocess.CalledProcessError(return_code, cmd)

#* String
def lf(string: str, lvl: int = 1) -> str:
    fr = sys._getframe()
    for _ in range(lvl):
        assert fr.f_back is not None
        fr = fr.f_back
    vars_dict = fr.f_globals.copy()
    vars_dict.update(fr.f_locals)
    return string.format(**vars_dict)

#* Regex
def re_filter(regex, seq):
    return list(filter(lambda s: re.search(regex, s), seq))

def re_seq(regex, s):
    return re.findall(regex, s)

def re_find(regex, s):
    rs = re_seq(regex, s)
    if len(rs) == 1:
        return rs[0]
    elif len(rs) == 0:
        raise RuntimeError("Could not find regex", regex, s)
    else:
        raise RuntimeError("Multiple matches for regex", regex, s)

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
