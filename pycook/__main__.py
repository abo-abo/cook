import sys
import os
import pycook as pc
import elisp as el

def script_describe(book):
    return "usage: cook <recipe>\n\nAvailable recipes:\n" + \
        pc.recipe_names(book)

def script_get_book():
    if el.file_exists_p("Cookbook.py"):
        return os.path.realpath("Cookbook.py")
    elif el.file_exists_p("cook/Cookbook.py"):
        return os.path.realpath("cook/Cookbook.py")
    else:
        raise RuntimeError("No Cookbook.py or cook/Cookbook.py found")

def main(argv = None):
    if argv is None:
        argv = sys.argv
    try:
        book = script_get_book()
        pc.main(argv, book)
    except RuntimeError as e:
        print(e)
