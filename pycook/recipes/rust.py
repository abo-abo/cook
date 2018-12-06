#* Imports
import pycook.elisp as el
lf = el.lf

#* Functions
def compile_and_run(src_file):
    exe_file = el.file_name_sans_extension(src_file)
    if (el.file_exists_p(exe_file) and
        el.file_newer_than_file_p(exe_file, src_file)):
        return [lf("./{exe_file}")]
    else:
        return [
            lf("rustc {src_file}"),
            lf("./{exe_file}")]
