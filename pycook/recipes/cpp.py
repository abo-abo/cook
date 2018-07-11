#* Imports
import sys
import re
import pycook.elisp as el
import pycook.cook as cook
lf = el.lf
sc = el.sc

#* Functions
def lib_name(fname):
    return "-l:" + el.file_name_nondirectory(fname)

def compile_and_run(inputs, std = "c++11", flags = "", idirs = [], libs = []):
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
    libs_inl = [x for x in inputs if x in diff]
    libs_str = " " + " ".join(["-l" + lib for lib in libs_inl])
    lib_dirs = set(el.mapcar(el.file_name_directory, libs))
    lflags = el.mapcar(lib_name, libs)
    libs_str += " " + " ".join(["-L" + d for d in lib_dirs] + lflags)
    exe_file = re.sub("cc$", "e", main_file)
    res = []
    flags += " " + " ".join(["-I" + d for d in idirs])
    if (not el.file_exists_p(exe_file) or
        any([el.file_newer_than_file_p(f, exe_file) for f in sources + [cook.script_get_book()[0]]])):
        ccmd = lf("g++ -g -O2 -std={std} {flags} -o {exe_file} ") + " ".join(sources) + libs_str
        res += [ccmd]
    if lib_dirs:
        res += ["export LD_LIBRARY_PATH=" + ":".join(lib_dirs)]

    res += [lf("./{exe_file}")]
    return res
