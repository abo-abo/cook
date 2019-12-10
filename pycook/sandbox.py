#* Description
__doc__ = "Run an arbitary amount of commands inside Docker."

#* Imports
import sys
import os
import re
import argparse
import json
import pycook.elisp as el
lf = el.lf
dd = el.default_directory()

#* Functions
def docker_opt_mount(fr, to, opts="rw"):
    return lf("-v {fr}:{to}:{opts}")

def docker_opt_env_mount(v, d):
    return lf("-e {v}={d} ") + docker_opt_mount(d, d)

def docker_opt_tty():
    if sys.stdout.isatty():
        return "-it"
    else:
        return "-i"

def docker_opt_cleanup():
    return "--rm"

def docker_opt_all_ports():
    return "--net=host"

def docker_opt_user():
    return "-e USER=$(id -u) -u $(id -u):$(id -g)"

def docker_opt_display():
    return "-e DISPLAY=$DISPLAY"

def docker_run(image, args, mount=None, env_mount=None, flags=()):
    cmds_mount = []
    cmds_env_mount = []
    if mount:
        for (f1, f2) in mount:
            cmds_mount += [docker_opt_mount(el.expand_file_name(f1), el.expand_file_name(f2))]
    if env_mount:
        for (d, e) in env_mount:
            el.make_directory(d)
            cmds_env_mount += [docker_opt_env_mount(e, el.expand_file_name(d))]
    cmds = [
        "docker run",
        "-i" if "-T" in flags else docker_opt_tty(),
        docker_opt_cleanup(),
        "" if "-H" in flags else docker_opt_all_ports(),
        docker_opt_user(),
        docker_opt_display(),
        docker_opt_mount(dd, dd),
        "-w " + dd]
    cmds += cmds_env_mount
    cmds += cmds_mount
    cmds += [
        docker_opt_mount("/etc/group", "/etc/group", "ro"),
        docker_opt_mount("/etc/passwd", "/etc/passwd", "ro"),
        docker_opt_mount(dd, dd),
        lf("{image} bash -e -c '{args}'")]
    return " ".join(cmds)

class ArgList:
    def __init__(self):
        self.E = []
        self.m = []
        self.docker_image = None
        self.cmds = ""
        self.flags = []

    def __repr__(self):
        return " ".join((
            ["sandbox"] +
            [lf("-E {x[0]} {x[1]}") for x in self.E] +
            [lf("-m {x[0]} {x[1]}") for x in self.m] +
            [self.docker_image or "?"] +
            [self.cmds or ""]))

def get_args(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("docker_image")
    parser.add_argument("cmds", nargs="?", help="shell arguments", default="bash")
    parser.add_argument(
        "-m",
        action="append",
        nargs=2,
        metavar=("file_fr", "file_to"),
        help="mount file_fr on host to file_to in docker")
    parser.add_argument(
        "-E",
        action="append",
        nargs=2,
        metavar=("file", "env"),
        help="mount file on host to itself in docker and point env to it")
    parser.add_argument(
        "-H", help="disable --net=host")
    parser.add_argument(
        "-~", help="mount $HOME")
    if len(argv) < 1:
        return parser.parse_args(argv)
    args = ArgList()
    xs = list(reversed(argv))
    while xs:
        x = xs.pop()
        if x == "-E":
            args.E += [(xs.pop(), xs.pop())]
        elif x == "-~":
            args.E += [(os.path.expanduser("~"), "HOME")]
        elif x == "-m":
            args.m += [(xs.pop(), xs.pop())]
        elif re.match("-[a-zA-Z]", x):
            args.flags.append(x)
        else:
            args.docker_image = x
            if xs:
                args.cmds = " ".join(reversed(xs))
            else:
                args.cmds = "bash"
            break
    return args


def get_image(i):
    if i == "." and el.file_exists_p("params.json"):
        js = json.load(open("params.json", "r"))
        return js["docker_url"] + ":" + js["docker_version"]
    else:
        return i

def main():
    argv = sys.argv[1:]
    if len(argv) == 0:
        argv = ["-h"]
        get_args(argv)
    args = get_args(argv)
    cmd = docker_run(
        get_image(args.docker_image), args.cmds,
        args.m, args.E, args.flags)
    el.bash(cmd)
