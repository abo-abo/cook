# * Includes
import sys
import os
import re
import subprocess
import ast
import inspect
import collections
import pycook.elisp as el
import pycook.insta as st
import pathlib
from pycook import recipes
from typing import List
import types

lf = el.lf

# * Globals
start_dir = el.default_directory()


# * Functions
def recipe_p(x):
    try:
        return inspect.getfullargspec(x[1]).args[0] == "recipe"
    except:  # noqa
        return None


def load_module(path: str) -> types.ModuleType:
    from importlib.machinery import SourceFileLoader

    name = pathlib.Path(path).stem
    return SourceFileLoader(name, path).load_module()


def recipe_names_ordered(book):
    body = ast.parse(st.slurp(book)).body
    fns = [f for f in body if isinstance(f, ast.FunctionDef)]
    return [fn.name for fn in fns]


def recipe_dict(book):
    d = el.file_name_directory(book)
    if d not in sys.path:
        sys.path.append(d)
    mod = load_module(book)
    funs = inspect.getmembers(mod, inspect.isfunction)
    funs = filter(recipe_p, funs)
    names = recipe_names_ordered(book)
    items = sorted(funs, key=lambda x: el.position(x[0], names, 42))
    return collections.OrderedDict(items)


def book_config(book):
    rc_file = el.expand_file_name("~/.cook.d/__config__.py")
    if el.file_exists_p(rc_file):
        mod = load_module(rc_file)
        config = mod.config
        if book in config:
            return config[book]
        elif "*" in config:
            return config["*"]
    return {}


def recipe_args_description(f):
    spec = inspect.getfullargspec(f)
    res = []
    ld = len(spec.defaults) if spec.defaults else 0
    if ld:
        di = dict(zip(spec.args, [*[None] * (len(spec.args) - ld), *spec.defaults]))
    else:
        di = dict.fromkeys(spec.args, None)
    d = len(spec.args) - ld - 1

    for i, a in enumerate(spec.args[1:]):
        if a == "config":
            if di[a].get("vterm"):
                res.append(":vterm=True")
            continue
        if i >= d:
            default = di[a]
            if isinstance(default, list):
                s = ":" + a + "=(" + "|".join(default) + ")"
            else:
                s = ":" + a + "=" + repr(default)
            res.append(s)
        else:
            res.append(":" + a + "=''")
    return " " + " ".join(res)


def function_names_ordered(book):
    body = ast.parse(st.slurp(book)).body
    return [f for f in body if isinstance(f, ast.FunctionDef)]


def functiondef_recipe_description(fn):
    res = []
    name = fn.name
    spec = fn.args
    ld = len(spec.defaults)
    d = len(spec.args) - ld - 1
    for i, a in enumerate(spec.args[1:]):
        if i >= d:
            res.append(":" + a.arg + "=" + spec.defaults[i - d].value)
        else:
            res.append(":" + a.arg + "=''")
    if res:
        return name + " " + " ".join(res)
    else:
        return name


def recipe_names(book):
    di = recipe_dict(book)
    ns = [k + recipe_args_description(v) for (k, v) in di.items()]
    return "\n".join(ns)

    # fns = function_names_ordered(book)
    # rs = [fn for fn in fns if fn.args.args and fn.args.args[0].arg == "recipe"]
    # return "\n".join(functiondef_recipe_description(fn) for fn in rs)


def describe(book, module=""):
    res = "Usage: cook [options] [book]"
    if module:
        res += " :" + module
    res += " <recipe>\n"
    res += "\nBook: " + book + "\n"
    names = "\n".join(["  " + n for n in recipe_names(book).split("\n")])
    return (
        res
        + "\nRecipes:\n"
        + names
        + """

Options:
  -h, --help                  Show help.
  --list                      List only the recipes, separated by newlines.
  -p                          Print commands instead of running them."""
    )


def script_get_book():
    dd = el.default_directory()
    d1 = el.locate_dominating_file(dd, "Cookbook.py")
    d2 = el.locate_dominating_file(dd, "cook/Cookbook.py")
    if d1 and (not d2 or len(d1) > len(d2) - 5):
        return (d1, el.file_name_directory(d1))
    elif d2:
        return (d2, el.file_name_directory(el.file_name_directory(d2)))
    else:
        raise RuntimeError("No Cookbook.py or cook/Cookbook.py found")


def get_book():
    caller_frame = sys._getframe().f_back.f_back
    caller_file = caller_frame.f_code.co_filename
    book = os.path.realpath(caller_file)
    return book


def log_file_name(base_dir, book, recipe):
    sub_dir = "_".join(el.delete("", os.path.normpath(book).split(os.sep)[:-1]))
    full_dir = el.expand_file_name(sub_dir, base_dir)
    el.make_directory(full_dir)
    ts = el.replace_regexp_in_string(" ", "_", el.timestamp())
    name = f"{ts}-{recipe}.txt"
    return el.expand_file_name(name, full_dir)


def recipe_arity(f):
    spec = inspect.getfullargspec(f)
    args = [*spec.args, spec.varargs] if spec.varargs else spec.args
    res = len(args) - 1
    if spec.defaults:
        res -= len(spec.defaults)
    return res


class CommandLog:
    def __init__(self):
        self.cmds = []
        self.current_host = None

    def record(self, cmd, desc=None):
        if desc:
            (host, cmd) = desc
        else:
            host = el.HOST
        m = re.match("^cat (.*)$", cmd)
        if m:
            if self.cmds and self.cmds[-1] == "# stat " + m.group(1):
                self.cmds.pop()
        if host != self.current_host:
            self.current_host = host
            if host:
                self.cmds.append("# ssh " + host)
            else:
                self.cmds.append("# exit")
        self.cmds.append("# " + re.sub("\n", "\\\\n", cmd))


def get_fun_cfg(fun):
    spec = inspect.getfullargspec(fun)
    if "config" in spec.args:
        ld = len(spec.defaults)
        d = len(spec.args) - ld
        i = spec.args.index("config")
        fun_cfg = spec.defaults[i - d]
    else:
        fun_cfg = {}
    return fun_cfg


def _main(book, module, flags, args):
    if len(args) == 0:
        print(describe(book, module))
    elif args[0] == "--list":
        print(recipe_names(book))
    elif args[0] == "--help":
        print(describe(book))
    else:
        recipe = args[0]
        fun = recipe_dict(book)[recipe]
        fun_cfg = get_fun_cfg(fun)
        cfg = book_config(book)
        old_sc_hookfn = el.sc_hookfn
        log = CommandLog()
        el.sc_hookfn = log.record
        try:
            if "-p" in flags:
                sys.stdout = open(os.devnull, "w", encoding="utf-8")
            ret_cmds = fun(42, *recipe_args(fun, args[1:])) or []
        except:  # noqa
            if cfg.get("pdb", False):
                import pdb

                pdb.set_trace()
                return
            else:
                raise
        el.sc_hookfn = old_sc_hookfn
        if "-l" in flags:
            sys.stdout = sys.__stdout__
            print("\n".join([cmd[2:] for cmd in log.cmds] + ret_cmds))
        else:
            if "-s" not in flags:
                if len(log.cmds) < 10:
                    spec = inspect.getfullargspec(fun)
                    if (
                        ("log" in spec.args)
                        and spec.defaults
                        and spec.defaults[spec.args.index("log") - 1] is None
                    ):
                        pass
                    elif log.cmds:
                        print("\n".join(log.cmds))
            if ret_cmds:
                dd = re.search("^(.*/)(cook/?)Cookbook.py$", book)
                if dd:
                    os.chdir(dd.group(1))
                if "pty" in fun_cfg:
                    if isinstance(ret_cmds, str):
                        cmd = ret_cmds
                    else:
                        cmd = "\n".join(ret_cmds)
                    from pycook.pty import make_runner
                    runner = make_runner(" ".join([module, *args]))
                    r = runner.run(
                        cmd,
                        pty="pty" in fun_cfg,
                        echo=True,
                        env=os.environ | {"HISTFILE": runner.history_fname},
                    )
                    if "tee" in cfg:
                        basedir = os.path.expanduser(cfg["tee"]["location"])
                        fname = log_file_name(basedir, book, recipe)
                        el.barf(
                            fname,
                            f"Book: {book}\nRecipe: {recipe}\n"
                            + r.stdout.replace("\r", ""),
                        )
                else:
                    el.bash(ret_cmds, echo=True)


def modules(full=False, match=False):
    cook_dir = el.file_name_directory(recipes.__file__)
    cook_modules = el.directory_files(cook_dir, full, match)
    user_dir = el.expand_file_name("~/.cook.d")
    if el.file_exists_p(user_dir):
        df = el.directory_files(user_dir, full, match)
        user_modules = [
            f
            for f in df
            if os.path.isfile(el.expand_file_name(f, user_dir))
            and os.path.splitext(f)[1] == ".py"
        ]
    else:
        user_modules = []
    cook_modules += user_modules
    return list(filter(lambda s: not re.search("__", s), cook_modules))


def module_names():
    ms = modules(False, "[^_]\\.py$")
    return el.delete_dups([s[:-3] for s in ms])


def recipe_args(f, args_provided):
    spec = inspect.getfullargspec(f)
    args_req = spec.args
    assert args_req[0] == "recipe"
    if len(args_provided) >= 1 and re.match(r":\w+=", args_provided[0]):
        res = []
        for arg in args_provided:
            m = re.match(f"^:\\w+=(.*)$", arg)
            assert m
            res.append(m.group(1))
        return res
    if len(args_provided) >= 2 and args_provided[0].startswith(":"):
        res = []
        for k, v in el.partition(2, args_provided):
            if k != "config":
                res.append(v)
        return res
    if len(args_req) == 2 and args_req[1] == "config":
        config = {}
        for k, v in el.partition(2, args_provided):
            config["select_" + k[1:]] = v
        return [config]
    args_missing = args_req[1 + len(args_provided) :]
    if spec.defaults:
        args_missing = args_missing[: -len(spec.defaults)]
    args_input = []
    for a in args_missing:
        args_input.append(input(a + ": "))
    return args_provided + args_input


def parse_flags(argv):
    res = []
    i = 1
    for x in argv[1:]:
        if re.match("-[^-]", x):
            res.append(x)
            i += 1
        else:
            break
    return (res, argv[i:])


def main(argv=None):
    if argv is None:
        argv = sys.argv
    try:
        (flags, rest) = parse_flags(argv)
        if rest == [":"]:
            for module in modules(True):
                print(module)
            sys.exit(0)
        if len(rest) >= 1 and re.match("^:", rest[0]):
            module = rest[0][1:]
            book = get_module(module)
            args = rest[1:]
        else:
            module = ""
            (book, dd) = script_get_book()
            os.chdir(dd)
            args = rest
        sys.path.append(el.file_name_directory(book))
        _main(book, module, flags, args)
    except subprocess.CalledProcessError as e:
        # print(e)
        sys.exit(e.returncode)
    except KeyboardInterrupt:
        sys.exit(1)
    except RuntimeError as e:
        print(e)
        sys.exit(1)


def get_module(name):
    """Load a module NAME.
    If two modules are on sys.path, prefer the one on ~/.cook.d/.
    """
    mods = modules(True, name)
    if len(mods) == 2:
        return el.re_filter("\\.cook\\.d/", mods)[0]
    else:
        assert len(mods) == 1, mods
        return mods[0]


def completions(argv: List[str]) -> str:
    assert argv[0] == "cook"
    (_flags, args) = parse_flags(argv)

    # below, assume we're completing the last word
    # current word being completed is sys.argv[-1]
    args = args[:-1]

    # fix the difference between bash-completion.el and the actual bash completion
    if re.match(":.", args[0]):
        args = [":", args[0][1:]] + args[1:]

    if len(args) == 1:
        if args[0] == ":":
            return "\n".join(module_names())
        else:
            rs = el.sc("cook --list").split("\n")
            # remove extra args
            rs = [re.split(" :", s)[0] for s in rs]
            fr = [r for r in rs if re.match(args[0], r)]
            return "\n".join(fr)
    if len(args) == 2 and args[0] == ":":
        matching_cands = el.re_filter("^" + args[1], module_names())
        return "\n".join(matching_cands)
    if len(args) == 3 and args[0] == ":":
        mod = get_module(args[1])
        cands = list(recipe_dict(mod).keys())
        matching_cands = el.re_filter("^" + args[2], cands)
        return "\n".join(matching_cands)
    if args[0] != ":":
        return ""
    mod = get_module(args[1])
    fun = recipe_dict(mod)[args[2]]
    part = args[-1]
    arg_idx = len(args) - 3
    return completions_idx(fun, arg_idx, part)


def completions_idx(fun, arg_idx, part):
    spec = inspect.getfullargspec(fun)
    if spec.varargs:
        fun_args = [*spec.args, spec.varargs]
    else:
        fun_args = spec.args

    if arg_idx > len(fun_args) - 1:
        return ""
    if fun_args[arg_idx] in ["fname", "fnames"]:
        return el.sc("compgen -f -- {part}")
    ld = len(spec.defaults) if spec.defaults else 0
    if arg_idx >= len(fun_args) - ld:
        arg_default = spec.defaults[arg_idx - len(fun_args) + len(spec.defaults)]
        if isinstance(arg_default, list):
            regex = "^" + part
            return "\n".join([c for c in arg_default if re.match(regex, c)])
    empty_args = [""] * recipe_arity(fun)
    comps = fun(("complete", part), *empty_args)
    if isinstance(comps, str):
        return comps
    else:
        regex = "^" + part
        return "\n".join([c for c in comps if re.match(regex, c)])


def complete(argv=None):
    if argv is None:
        argv = sys.argv
    with open("/tmp/cook_complete_argv", "w") as f:
        f.write(f"argv: {argv}")
    print(completions(argv[1:]))
