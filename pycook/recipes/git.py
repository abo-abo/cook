#* Imports
import os
import re
import time
from datetime import datetime
import pycook.elisp as el
sc = el.sc
lf = el.lf

#* Classes
class dd:
    def __init__(self, d):
        self.d = el.expand_file_name(d)
        self._old_dir = None

    def __enter__(self):
        self._old_dir = el.default_directory()
        os.chdir(self.d)

    def __exit__(self, *_):
        os.chdir(self._old_dir)

#* Functions
def repo_p(d):
    return el.file_exists_p(el.expand_file_name(".git", d))

def clean_p(repo):
    with dd(repo):
        out = sc("git status")
    return (
        re.search("nothing to commit, working directory clean", out) or
        re.search("nothing added to commit", out))

def git_time_to_datetime(s):
    t1 = time.strptime(s)
    t2 = time.mktime(t1)
    return datetime.fromtimestamp(t2)

def mtime(repo):
    """Return the last modification time of REPO."""
    if clean_p(repo):
        with dd(repo):
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
