#* Imports
import sys
import re
import pycook.elisp as el
import pycook.cook as cook
lf = el.lf

#* Functions
def compile_and_run(inputs, flags = ""):
    """Compile INPUTS together into an executable and run it.

    INPUTS is a list of source files, headers and libraries.
    The executable is named after the first source file.
    Any file that doesn't exist is assumed to be a library.
    """
    if type(inputs) is list:
        main_file = inputs[0]
    else:
        main_file = inputs
    assert(el.file_exists_p(main_file))
    sources = el.re_filter("(cc|cpp|h|hh|hpp)$", inputs)
    diff = set(inputs) - set(sources)
    libs = [x for x in inputs if x in diff]
    libs_str = " " + " ".join(["-l" + lib for lib in libs])
    exe_file = re.sub("cc$", "e", main_file)
    res = []
    if (not el.file_exists_p(exe_file) or
        any([el.file_newer_than_file_p(f, exe_file) for f in sources + [cook.script_get_book()[0]]])):
        res += [lf("g++ -g -O2 -std=c++11 {flags} -o {exe_file} ") + " ".join(sources) + libs_str]
    res += [lf("./{exe_file}")]
    return res
