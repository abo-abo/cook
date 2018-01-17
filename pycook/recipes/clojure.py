#* Imports
import os
import pycook.cook as pc
import pycook.elisp as el
import pycook.recipes.git as git
import pycook.recipes.java as java
from datetime import datetime
lf = el.lf

#* Recipes
def ng_install(recipe):
    local = "~/git/java/nailgun"
    jar = el.expand_file_name("nailgun-server/target/nailgun-server-0.9.2-SNAPSHOT.jar", local)
    res = git.clone("https://github.com/facebook/nailgun", local)
    if pc.stale("/usr/local/bin/ng", local):
        res += [lf("cd {local}"), "sudo make install"]
    if pc.stale(jar, local):
        res += [lf("cd {local}"), "mvn clean install"]
    res += java.addpath(jar)
    return res

def ng_server(recipe):
    return ["java com.martiansoftware.nailgun.NGServer"]

def ng_clojure(recipe):
    return ["ng clojure.main"]
