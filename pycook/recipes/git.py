#* Imports
import os
import re
import pycook.elisp as el
import time
from datetime import datetime
sc = el.sc
lf = el.lf

#* Functions
def repo_p(d):
    return el.file_exists_p(el.expand_file_name(".git", d))

def clean_p(repo):
    with el.dd(repo):
        out = sc("git status")
    if (re.search("nothing to commit, working directory clean", out) or
        re.search("nothing added to commit", out)):
        return True
    else:
        return False

def git_time_to_datetime(s):
    t1 = time.strptime(s)
    t2 = time.mktime(t1)
    return datetime.fromtimestamp(t2)

def mtime(repo):
    """Return the last modification time of REPO."""
    if clean_p(repo):
        with el.dd(repo):
            res = git_time_to_datetime(
                el.sc("git log -1 --date=local --format=%cd"))
        return res
    else:
        return datetime.now()

def clone(remote, local):
    res = []
    local = el.expand_file_name(local)
    if el.file_exists_p(local):
        if el.file_exists_p(el.expand_file_name(".git", local)):
            res += [
                "cd " + local,
                "git pull"]
        else:
            raise RuntimeError("Directory exists and is not a git repo", local)
    else:
        (bd, repo) = os.path.split(local)
        el.make_directory(bd)
        res += [
                lf("cd {bd}"),
                lf("git clone {remote} {repo}")]
    return res

#* Recipes
def clean(recipe):
    return ["git reflog expire --expire=now --all && git gc --aggressive --prune=now"]
