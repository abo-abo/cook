#* Imports
import re
import pycook.elisp as el
lf = el.lf

#* Recipes
def compile_and_run(recipe, source_file):
    class_file = re.sub("java$", "class", source_file)
    assert el.file_exists_p(source_file)
    source = el.file_name_nondirectory(source_file)
    path = el.file_name_directory(source_file)
    name = el.file_name_sans_extension(source)
    res = []
    if (not el.file_exists_p(class_file) or
            el.file_newer_than_file_p(source_file, class_file)):
        res += [lf("javac {source_file}")]
    res += [lf("java -cp {path} {name}")]
    return res
