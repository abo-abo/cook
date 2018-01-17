#* Imports
import sys
import re
import os
import pycook.elisp as el
lf = el.lf

#* Functions
def compile_and_run(source_file):
    class_file = re.sub("java$", "class", source_file)
    assert(el.file_exists_p(source_file))
    source = el.file_name_nondirectory(source_file)
    path = el.file_name_directory(source_file)
    name = el.file_name_sans_extension(source)
    res = []
    if (not el.file_exists_p(class_file) or
        el.file_newer_than_file_p(source_file, class_file)):
        res += [lf("javac {source_file}")]
    res += [lf("java -cp {path} {name}")]
    return res

def addpath(p):
    cp = os.getenv("CLASSPATH")
    if cp:
        ps = cp.split(":")
    else:
        ps = []
    if p in ps:
        return []
    else:
        ps.append(p)
        cp = ":".join(ps)
        cmd = lf("export CLASSPATH=\"{cp}\"")
        return [lf("echo '{cmd}' >> ~/.bashrc")]
