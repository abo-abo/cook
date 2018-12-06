#* Includes
import sys
import os
import re
import subprocess
import imp
import ast
import inspect
import collections
from datetime import datetime
import pycook.elisp as el
import pycook.recipes as recipes
import pycook.recipes.git as git
lf = el.lf

#* Globals
start_dir = el.default_directory()

#* Functions
def mtime(f):
    f = el.expand_file_name(f)
    if el.file_directory_p(f):
        if git.repo_p(f):
            return git.mtime(f)
        else:
            raise RuntimeError("Directory not a git repo", f)
    elif el.file_exists_p(f):
        return datetime.fromtimestamp(os.path.getmtime(f))
    else:
        return datetime.now()

def stale(target, *deps):
    mtarget = mtime(target)
    return any([mtime(dep) > mtarget for dep in deps])

def recipe_p(x):
    try:
        return inspect.getargspec(x[1]).args[0] == "recipe"
    except:
        return None

def recipe_names_ordered(book):
    body = ast.parse(el.slurp(book)).body
    return [f.name for f in body if isinstance(f, ast.FunctionDef)]

def recipe_dict(book):
    mod = imp.load_source("Cookbook", book)
    funs = inspect.getmembers(mod, inspect.isfunction)
    funs = el.cl_remove_if_not(recipe_p, funs)
    names = recipe_names_ordered(book)
    items = sorted(funs, key = lambda x: el.position(x[0], names, 42))
    return collections.OrderedDict(items)

def book_config(book):
    rc_file = el.expand_file_name("~/.cookrc.py")
    if el.file_exists_p(rc_file):
        mod = imp.load_source("book_config", rc_file)
        config = mod.config
        if book in config:
            return config[book]
        elif "*" in config:
            return config["*"]
    return {}

def recipe_name(f):
    arity = recipe_arity(f)
    if arity == 0:
        return f.__name__
    else:
        args = " ".join([":" + a for a in function_arglist(f)[1:]])
        return lf("{f.__name__} {args}")

def recipe_names(book):
    di = recipe_dict(book)
    ns = [recipe_name(v) for v in di.values()]
    return "\n".join(ns)

def describe(book):
    return \
        "usage: cook <recipe>\n\nAvailable recipes:\n" + \
        recipe_names(book)

def script_get_book():
    dd = el.default_directory()
    d1 = el.locate_dominating_file(dd, "Cookbook.py")
    d2 = el.locate_dominating_file(dd, "cook/Cookbook.py")
    if d1 and (not d2 or len(d1) > len(d2) - 5):
        return (d1, el.file_name_directory(d1))
    elif d2:
        return (d2, el.file_name_directory(el.file_name_directory(d2)))
    else:
        raise RuntimeError("No Cookbook.py or cook/Cookbook.py found")

def get_book():
    caller_frame = sys._getframe().f_back.f_back
    caller_file = caller_frame.f_code.co_filename
    book = os.path.realpath(caller_file)
    return book

def log_file_name(basedir, recipe):
    ts = el.replace_regexp_in_string(" ", "_", el.timestamp())
    name = el.lf("{ts}-{recipe}.txt")
    return el.expand_file_name(name, basedir)

def function_arglist(f):
    try:
        return inspect.getfullargspec(f).args
    except:
        return inspect.getargspec(f).args

def recipe_arity(f):
    return len(function_arglist(f)) - 1

def _main(argv, book):
    if len(argv) == 2:
        if argv[1] == "--list":
            print(recipe_names(book))
        else:
            recipe = argv[1]
            fun = recipe_dict(book)[recipe]
            cfg = book_config(book)
            if "tee" in cfg and recipe != "bash":
                basedir = cfg["tee"]["location"]
                fname = log_file_name(basedir, recipe)
                el.spit(lf("Book: {book}\nRecipe: {recipe}\n"), fname)
                tee = subprocess.Popen(["tee", "-a", fname], stdin = subprocess.PIPE)
                os.dup2(tee.stdin.fileno(), sys.stdout.fileno())
                os.dup2(tee.stdin.fileno(), sys.stderr.fileno())

            cmds = []
            old_sc_hookfn = el.sc_hookfn
            old_cd_hookfn = el.cd_hookfn
            el.sc_hookfn = lambda s: cmds.append("# " + re.sub("\n", "\\\\n", s))
            el.cd_hookfn = lambda d: cmds.append("# cd " + d)
            ret_cmds = fun(42) or []
            all_cmds = cmds + ret_cmds
            el.sc_hookfn = old_sc_hookfn
            el.cd_hookfn = old_cd_hookfn
            el.bash(all_cmds, echo = True)
    elif len(argv) == 3 and argv[1] == "--pipe":
        mod = imp.load_source("Cookbook", book)
        funs = dict(inspect.getmembers(mod, inspect.isfunction))
        fun = funs[argv[2]]
        os.chdir(start_dir)
        print(fun(input()))
    elif len(argv) >= 2 and recipe_arity(recipe_dict(book)[argv[1]]) == len(argv[2:]):
        recipe = argv[1]
        fun = recipe_dict(book)[recipe]
        el.bash(fun(42, *argv[2:]) or [], echo = True)
    else:
        print(describe(book))

def modules(full = False, match = False):
    cook_dir = el.file_name_directory(recipes.__file__)
    cook_modules = el.directory_files(cook_dir, full, match)
    cook_modules = el.filter(lambda s: not re.search("__\\.py", s), cook_modules)
    user_dir = el.expand_file_name("~/.cook.d")
    if el.file_exists_p(user_dir):
        user_modules = el.filter(os.path.isfile, el.directory_files(user_dir, full, match))
    else:
        user_modules = []
    return cook_modules + user_modules

def module_names():
    ms = modules(False, "[^_]\\.py$")
    return [s[:-3] for s in ms]

def recipe_args(f, args_provided):
    args_req = function_arglist(f)
    assert(args_req[0] == "recipe")
    args_missing = args_req[1 + len(args_provided):]
    args_input = []
    for a in args_missing:
        args_input.append(input(a + ": "))
    return args_provided + args_input

def main(argv = None):
    if argv is None:
        argv = sys.argv
    if (len(argv) >= 3 and
        re.match("^:", argv[1])):
        mods = modules(True, argv[1][1:])
        assert(len(mods) == 1)
        book = mods[0]
        recipe = argv[2]
        fun = recipe_dict(book)[recipe]
        cmds = fun(42, *recipe_args(fun, argv[3:])) or []
        el.bash(cmds)
        sys.exit(0)
    try:
        (book, dd) = script_get_book()
        os.chdir(dd)
        _main(argv, book)
    except subprocess.CalledProcessError as e:
        print(e)
        sys.exit(e.returncode)
    except RuntimeError as e:
        print(e)
        sys.exit(1)

def complete(argv = None):
    if argv is None:
        argv = sys.argv
    # with open("/tmp/cook.txt", "a") as f:
    #     f.write(str(argv) + "\n")
    assert(argv[1] == "cook")
    args = argv[2:-1]
    # current word being completed
    curr = argv[-1]
    # below, assume we're completing the last word

    # fix the difference between bash-completion.el and the actual bash completion
    if re.match(":.", args[0]):
        args = [":", args[0][1:]] + args[1:]
        if args[-1] in module_names():
            args += [""]

    if len(args) == 1:
        if args[0] == ":":
            print("\n".join(module_names()))
        else:
            rs = el.sc("cook --list").split("\n")
            # remove extra args
            rs = [re.split(" :", s)[0] for s in rs]
            fr = [r for r in rs if re.match(args[0], r)]
            print("\n".join(fr))
    elif len(args) == 2 and args[0] == ":":
        matching_cands = el.re_filter("^" + args[1], module_names())
        print("\n".join(matching_cands))
    elif len(args) == 3 and args[0] == ":":
        mod = modules(True, args[1])
        cands = list(recipe_dict(mod[0]).keys())
        matching_cands = el.re_filter("^" + args[2], cands)
        print("\n".join(matching_cands))
    else:
        print("")
