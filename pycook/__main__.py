#* Imports
import sys
import os
import subprocess
import pycook as pc
from pycook import elisp as el

#* Functions
def script_describe(book):
    return "usage: cook <recipe>\n\nAvailable recipes:\n" + \
        pc.cook.recipe_names(book)

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

def main(argv = None):
    if argv is None:
        argv = sys.argv
    try:
        (book, dd) = script_get_book()
        os.chdir(dd)
        pc.main(argv, book)
    except subprocess.CalledProcessError as e:
        print(e)
        sys.exit(e.returncode)
