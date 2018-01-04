#* Imports
import sys
import re
import pycook.elisp as el
lf = el.lf

#* Functions
def compile_and_run(source_file):
    assert(el.file_exists_p(source_file))
    exe_file = re.sub("cc$", "e", source_file)
    res = []
    if (not el.file_exists_p(exe_file) or
        el.file_newer_than_file_p(source_file, exe_file)):
        res += [lf("g++ -g -O2 -std=c++11 -o {exe_file} {source_file}")]
    res += [lf("./{exe_file}")]
    return res
