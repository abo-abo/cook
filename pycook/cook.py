#* Includes
import sys
import os
import subprocess
import imp
import inspect
from pycook import elisp as el
run = el.bash
lf = el.lf

#* Functions
def recipe_p(x):
    return inspect.getargspec(x[1]).args == ["recipe"]

def recipe_dict(book):
    mod = imp.load_source("Cookbook", book)
    funs = inspect.getmembers(mod, inspect.isfunction)
    return dict(el.cl_remove_if_not(recipe_p, funs))

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

def get_book():
    caller_frame = sys._getframe().f_back.f_back
    caller_file = caller_frame.f_code.co_filename
    book = os.path.realpath(caller_file)
    return book

def log_file_name(basedir, recipe):
    ts = el.replace_regexp_in_string(" ", "_", el.timestamp())
    name = el.lf("{ts}-{recipe}.txt")
    return el.expand_file_name(name, basedir)

def main(argv = None, book = None):
    if argv is None:
        argv = sys.argv
    if book is None:
        book = get_book()
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
                sep = "-"*80
                el.spit(lf("Book: {book}\nRecipe: {recipe}\n{sep}\n"), fname)
                tee = subprocess.Popen(["tee", "-a", fname], stdin = subprocess.PIPE)
                os.dup2(tee.stdin.fileno(), sys.stdout.fileno())
                os.dup2(tee.stdin.fileno(), sys.stderr.fileno())
            run(fun(42), echo = True)
    else:
        print(describe(book))
