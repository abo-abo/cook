#* Imports
import sys
import os
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
        if d2:
            if len(d1) > len(d2):
                return d1
            else:
                return d2
    else:
        if d2:
            return d2
        else:
            raise RuntimeError("No Cookbook.py or cook/Cookbook.py found")

def main(argv = None):
    if argv is None:
        argv = sys.argv
    try:
        book = script_get_book()
        os.chdir(el.file_name_directory(book))
        pc.main(argv, book)
    except RuntimeError as e:
        print(e)
