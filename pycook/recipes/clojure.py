#* Imports
import os
import json
import collections.abc
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

def update_in(d, u):
    for (k, v) in u.items():
        if isinstance(v, collections.abc.Mapping):
            d[k] = update_in(d.get(k, {}), v)
        else:
            d[k] = v
    return d

def outdated():
    try:
        j = json.loads(el.sc("cat ~/.lein/profiles.clj | jet --to json"))
        if (j["user"]["aliases"]["outdated"] == ['run', '-m', 'antq.core'] and
            ['com.github.liquidz/antq', '1.3.1'] in j["user"]["dependencies"]):
            print("antq: OK")
    except:
        j["user"]["dependencies"].append(['com.github.liquidz/antq', '1.3.1'])
        update_in(j, {
            'user': {'aliases': {'outdated': ['run', '-m', 'antq.core']}}})
        json.dump(j, open("/tmp/lein_profiles.json", "w"))
        st.bash("cat /tmp/lein_profiles.json | jet --from json -k -p > ~/.lein/profiles.clj")
