#* Includes
import sys
import os
import re
import subprocess
import imp
import inspect
import collections
import pycook.elisp as el
import pycook.recipes as recipes
lf = el.lf

#* Globals
start_dir = el.default_directory()

#* Functions
def recipe_p(x):
    return inspect.getargspec(x[1]).args == ["recipe"]

def recipe_dict(book):
    mod = imp.load_source("Cookbook", book)
    funs = inspect.getmembers(mod, inspect.isfunction)
    return collections.OrderedDict(el.cl_remove_if_not(recipe_p, funs))

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

def recipe_names(book):
    return "\n".join(recipe_dict(book).keys())

def describe(book):
    return \
        "usage: cook <recipe>\n\nAvailable recipes:\n" + \
        recipe_names(book)

def script_get_book():
    dd = el.default_directory()
    d1 = el.locate_dominating_file(dd, "Cookbook.py")
    d2 = el.locate_dominating_file(dd, "cook/Cookbook.py")
    if d1:
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

def _main(argv, book):
    if len(argv) == 2:
        if argv[1] == "--list":
            print(recipe_names(book))
        else:
            recipe = argv[1]
            fun = recipe_dict(book)[recipe]
            cfg = book_config(book)
            if "tee" in cfg:
                basedir = cfg["tee"]["location"]
                fname = log_file_name(basedir, recipe)
                el.spit(lf("Book: {book}\nRecipe: {recipe}\n"), fname)
                tee = subprocess.Popen(["tee", "-a", fname], stdin = subprocess.PIPE)
                os.dup2(tee.stdin.fileno(), sys.stdout.fileno())
                os.dup2(tee.stdin.fileno(), sys.stderr.fileno())

            cmds = []
            old_sc_hookfn = el.sc_hookfn
            old_cd_hookfn = el.cd_hookfn
            el.sc_hookfn = lambda s: cmds.append("# " + s)
            el.cd_hookfn = lambda d: cmds.append("# cd " + d)
            ret_cmds = fun(42)
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
    else:
        print(describe(book))

def main(argv = None):
    if argv is None:
        argv = sys.argv
    if (len(argv) == 3 and
        re.match("^:", argv[1])):
        d = el.file_name_directory(recipes.__file__)
        mods = el.directory_files(d, True, argv[1][1:])
        assert(len(mods) == 1)
        _main([argv[0], argv[2]], mods[0])
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
