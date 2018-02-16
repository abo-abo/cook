#* Imports
import sys
import re
import pycook.elisp as el
lf = el.lf

#* Functions
def compile_and_run(source_file):
    sources = source_file
    if type(source_file) is list:
        source_file = source_file[0]
    assert(el.file_exists_p(source_file))
    exe_file = re.sub("cc$", "e", source_file)
    res = []
    if (not el.file_exists_p(exe_file) or
        any([el.file_newer_than_file_p(f, exe_file) for f in sources])):
        res += [lf("g++ -g -O2 -std=c++11 -o {exe_file} ") + " ".join(sources)]
    res += [lf("./{exe_file}")]
    return res
