#* Imports
import os
import pycook.elisp as el
import pycook.insta as st
lf = el.lf

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
        return [lf("+export CLASSPATH=\"{cp}\"")]

#* Recipes
def ng_install(recipe):
    local = el.expand_file_name("~/git/clojure/nailgun/")
    st.git_clone("https://github.com/facebook/nailgun", local, "84f3b05")
    st.install_package("maven")
    jar = el.expand_file_name("nailgun-server/target/nailgun-server-1.0.0.jar", local)
    st.make(
        jar,
        [lf("cd {local}"), "mvn clean install"],
        [local])
    st.make(
        "/usr/local/bin/ng",
        [lf("cd {local}"), "sudo make install"])
    st.patch("~/.bashrc", addpath(jar))

def ng_server(recipe):
    return ["java com.facebook.nailgun.NGServer"]

def ng_clojure(recipe):
    return ["ng clojure.main"]
