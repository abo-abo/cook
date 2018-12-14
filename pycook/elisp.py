#* Imports
import subprocess
import sys
import os
import re
import getpass
import shlex
import collections
from datetime import datetime

#* Globals
sc_hookfn = None
cd_hookfn = None

#* Functional
def apply (function, arguments):
    """Call FUNCTION with ARGUMENTS, return the result."""
    return function (*arguments)

def mapcar (func, lst):
    """Compatibility function for Python3.

    In Python2 `map' returns a list, as expected.  But in Python3
    `map' returns a map object that can be converted to a list.
    """
    return list (map (func, lst))

def filter(pred, lst):
    return [x for x in lst if pred(x)]

cl_remove_if_not = filter

def cl_remove_if (pred, lst):
    return [x for x in lst if not pred(x)]

def cl_position_if (pred, lst):
    pos = 0
    for item in lst:
        if pred (item):
            return pos
        else:
            pos += 1

def position (item, lst, default = None):
    if item in lst:
        return lst.index(item)
    else:
        return default

def cl_set_difference(lst1, lst2):
    s = set(lst2)
    return [x for x in lst1 if x not in s]

def cl_find_if (pred, lst):
    for item in lst:
        if pred (item):
            return item

def position_if(pred, lst):
    for (i, item) in enumerate(lst):
        if pred(item):
            return i

def mapconcat (func, lst, sep):
    if func:
        return sep.join (map (func, lst))
    else:
        return sep.join (lst)

def flatten (seq):
    """Flatten a list of lists into a list."""
    return [item for sublist in seq for item in sublist]

def partition (n, seq):
    return [seq[i:i + n] for i in range (0, len (seq), n)]

def delete (element, lst):
    return [x for x in lst if x != element]

def delete_dups(lst):
    seen = set()
    seen_add = seen.add
    return [x for x in lst if not (x in seen or seen_add(x))]

#* Sys
def top_level ():
    f = sys._getframe ()
    while f.f_back:
        f = f.f_back
    return f

def crash ():
    tf = top_level ()
    f = sys._getframe ().f_back
    tf.f_globals["lnames"] = f.f_locals.keys ()
    for (k, v) in f.f_locals.items ():
        tf.f_globals[k] = v
    raise RuntimeError ("locals stored to globals")

#* OS
def addpath (path):
    sys.path.append (path)

def user_login_name ():
    return getpass.getuser()

def emacsclient_eval(expr):
    e = re.sub('"', "\\\"", expr)
    return lf('emacsclient -e "{e}"')

def eval (s):
    return shell_command_to_string(emacsclient_eval(s))

def beval(s, init_file = None):
    s = re.sub('"', "\\\"", s)
    if init_file:
        init = "-l "+ init_file
    else:
        init = ""
    return shell_command_to_string(lf('emacs -batch {init} --eval "(print {s})"'))

def load_file (f):
    "Load a Python file into the REPL."
    exec (open (f).read(), globals ())

#* Files
class dd:
    def __init__(self, d):
        self.d = expand_file_name(d)

    def __enter__(self):
        self._old_dir = default_directory()
        os.chdir(self.d)

    def __exit__(self, *_):
        os.chdir(self._old_dir)

def default_directory ():
    return os.getcwd ()

def locate_dominating_file (f, n):
    if file_directory_p(f):
        d = f
    else:
        d = file_name_directory(expand_file_name(f))
    while d != "/":
        nd = os.path.join(d, n)
        if file_exists_p(nd):
            return nd
        d = file_name_directory(d)

def cd (directory):
    d = expand_file_name (directory)
    if cd_hookfn:
        cd_hookfn(d)
    os.chdir (d)

def make_directory(d):
    """Work around Python2/3 `os.makedirs' incompat."""
    if not os.path.exists(d):
        os.makedirs(d)

def expand_file_name (f, directory = None):
    if not directory:
        directory = os.getcwd ()
    else:
        directory = os.path.expanduser(directory)
    if re.match ("^~", f):
        return os.path.expanduser (f)
    else:
        # return os.path.realpath (os.path.join (directory, f))
        return os.path.join(directory, f)

def file_name_sans_extension (f):
    return os.path.splitext (f)[0]

def file_name_directory (f):
    return os.path.dirname (f)

def file_name_nondirectory (f):
    return os.path.basename (f)

def file_name(f):
    return file_name_sans_extension(file_name_nondirectory(f))

def file_exists_p (f):
    return os.path.exists (expand_file_name(f))

def file_newer_than_file_p(f1, f2):
    return os.path.getctime(f1) > os.path.getctime(f2)

def file_directory_p (f):
    return os.path.isdir (f)

def abbreviate_file_name (f, d):
    if not d[-1] == "/":
        d = d + "/"
    m = re.match (d, f)
    if m:
        return f[m.end ():]
    else:
        m = re.match (f, d)
        if m:
            return ".".join (["../"]* (d[m.end ():].count ("/") - 1))

def directory_files (d, full = False, match = False):
    fl = os.listdir (d)
    if match:
        fl = cl_remove_if_not (lambda f:  None != string_match (match, f), fl)
    if full:
        fl = mapcar (lambda f: expand_file_name (f, d), fl)
    return fl

def delete_file (f):
    return os.remove (f)

#* File read/write
def slurp (f):
    fh = open (f, 'r')
    res = fh.read ()
    fh.close ()
    return res

def slurp_lines(f):
    return open(f, "r").read().splitlines()

def barf (f, s):
    fh = open (f, 'w')
    fh.write (s)
    fh.close ()

#* Shell
def shell_command_to_string (cmd):
    out = subprocess.check_output (["bash", "-c", cmd]).strip()
    if type(out) is str:
        return out
    else:
        return out.decode()

def sc(cmd):
    fcmd = lf(cmd, 2)
    if sc_hookfn:
        sc_hookfn(fcmd)
    return shell_command_to_string(fcmd)

def shell_command_to_list (cmd):
    cmd_output = shell_command_to_string (cmd)
    return [s for s in cmd_output.split ("\n") if s]

def sc_l(cmd):
    fcmd = lf(cmd, 2)
    return shell_command_to_list(fcmd)

def bash(cmd, echo = False, capture = False):
    if type(cmd) is list:
        cmd = "\n".join(cmd)
    if echo:
        sep = "-"*80
        print(sep)
        print("Run: \n" + cmd)
        print(sep)
    sys.stdout.flush()
    if capture:
        p = subprocess.Popen(["/bin/bash", "-e", "-c", cmd], stdout=subprocess.PIPE)
        (stdout, stderr) = p.communicate()
        if p.returncode == 0:
            return stdout.decode().strip()
        else:
            raise subprocess.CalledProcessError(p.returncode, cmd, stdout, stderr)
    else:
        p = subprocess.Popen(["/bin/bash", "-e", "-c", cmd])
        return_code = p.wait()
        if return_code == 0:
            return 0
        else:
            sys.stdout.flush()
            sys.stderr.flush()
            raise subprocess.CalledProcessError(return_code, cmd)

#* String
def lf (string, lvl = 1):
    fr = sys._getframe()
    for i in range(lvl):
        fr = fr.f_back
    vars_dict = fr.f_globals.copy()
    vars_dict.update(fr.f_locals)
    return string.format(**vars_dict)

#* Regex
def string_match (regexp, string):
    global match_data
    m = re.search (regexp, string)
    if m:
        match_data = m
        return m.start ()

def match_string (group):
    global match_data
    return match_data.group (group)

def match_beginning (group):
    global match_data
    return match_data.start (group)

def match_end (group):
    global match_data
    return match_data.end (group)

def re_filter (regex, seq):
    return list (filter (lambda s: re.search (regex, s), seq))

def re_seq (regex, s):
    return re.findall (regex, s)

def re_match(regex):
    return lambda s: re.search(regex, s)

def replace_regexp_in_string (regexp, rep, string):
    return re.sub (re.compile(regexp, re.MULTILINE), rep, string)

def spit(x, fname):
    with open(fname, "w") as f:
        if type(x) is str:
            f.write(x)
        elif type(x) is list and type(x[0]) is str:
            if re.search("\n", x[0]):
                sep = ""
            else:
                sep = "\n"
            f.write(sep.join(x))
        else:
            raise RuntimeError("Unexpected object", x)

def re_split(regex, lst):
    l1 = []
    l2 = []
    for x in lst:
        if re.search(regex, x):
            l1.append(x)
        else:
            l2.append(x)
    return (l1, l2)

def group_by(f, lst):
    res = collections.OrderedDict()
    for x in lst:
        fx = f(x)
        if fx in res:
            res[fx].append(x)
        else:
            res[fx] = [x]
    return res

def re_extract(regex, group = 1):
    def res(s):
        m = re.search(regex, s)
        if m:
            return m.group(group)
    return res

#* Time
def timestamp():
    t = datetime.now()
    year = t.year
    month = t.month
    day = t.day
    hour = t.hour
    minute = t.minute
    dow = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"][t.weekday()]
    return lf("<{year}-{month:02d}-{day:02d} {dow} {hour:02d}:{minute:02d}>")
