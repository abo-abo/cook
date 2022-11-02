import pycook.insta as st
import json

def clean_up_term_escapes(recipe, fname):
    return f"sed -i 's/\x1b\\[[0-9;]*m//g' '{fname}'"

def to_utf8(recipe, fname):
    try:
        j = json.loads(st.run(f"dfeal '{fname}'"))
    except:
        print("npm i -g detect-file-encoding-and-language")
    encoding = j["encoding"]
    if encoding == "UTF-8":
        print("Already in UTF-8")
    else:
        return f"iconv -f {encoding} -t utf-8 '{fname}' | sponge '{fname}'"
