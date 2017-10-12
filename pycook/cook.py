#* Includes
import sys
import os
import imp
import inspect
from pycook import elisp as el
run = el.bash

#* Functions
def recipe_p(x):
    return inspect.getargspec(x[1]).args == ["recipe"]

def recipe_dict(book):
    mod = imp.load_source("Cookbook", book)
    funs = inspect.getmembers(mod, inspect.isfunction)
    return dict(el.cl_remove_if_not(recipe_p, funs))

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

def main(argv = None, book = None):
    if argv is None:
        argv = sys.argv
    if book is None:
        book = get_book()
    if len(argv) == 2:
        if argv[1] == "--list":
            print(recipe_names(book))
        else:
            fun = recipe_dict(book)[argv[1]]
            run(fun(42))
    else:
        print(describe(book))
