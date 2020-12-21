#* Includes
import sys
import os
import re
import subprocess
import imp
import ast
import inspect
import collections
import pycook.elisp as el
import pycook.recipes as recipes
lf = el.lf

#* Globals
start_dir = el.default_directory()

#* Functions
def recipe_p(x):
    try:
        return inspect.getfullargspec(x[1]).args[0] == "recipe"
    except:
        return None

def recipe_names_ordered(book):
    body = ast.parse(el.slurp(book)).body
    return [f.name for f in body if isinstance(f, ast.FunctionDef)]

def recipe_dict(book):
    mod = imp.load_source("Cookbook", book)
    funs = inspect.getmembers(mod, inspect.isfunction)
    funs = filter(recipe_p, funs)
    names = recipe_names_ordered(book)
    items = sorted(funs, key=lambda x: el.position(x[0], names, 42))
    return collections.OrderedDict(items)

def book_config(book):
    rc_file = el.expand_file_name("~/.cook.d/__config__.py")
    if el.file_exists_p(rc_file):
        mod = imp.load_source("book_config", rc_file)
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
    d = len(spec.args) - ld - 1

    for (i, a) in enumerate(spec.args[1:]):
        if i >= d:
            res.append(":" + a + "=" + str(spec.defaults[i - d]))
        else:
            res.append(":" + a)
    return " " + " ".join(res)

def recipe_names(book):
    di = recipe_dict(book)
    ns = [k + recipe_args_description(v) for (k, v) in di.items()]
    return "\n".join(ns)

def describe(book, module=""):
    res = "Usage: cook [options] [book]"
    if module:
        res += " :" + module
    res += " <recipe>\n"
    res += "\nBook: " + book + "\n"
    names = "\n".join(["  " + n for n in recipe_names(book).split("\n")])
    return res + "\nRecipes:\n" + names + """

Options:
  -h, --help                  Show help.
  --list                      List only the recipes, separated by newlines.
  -p                          Print commands instead of running them."""

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
    name = el.lf("{ts}-{recipe}.txt")
    return el.expand_file_name(name, full_dir)

def function_arglist(f):
    spec = inspect.getfullargspec(f)
    if spec.varargs:
        return [*spec.args, spec.varargs]
    else:
        return spec.args

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
        cfg = book_config(book)
        if "tee" in cfg and recipe != "bash":
            basedir = os.path.expanduser(cfg["tee"]["location"])
            fname = log_file_name(basedir, book, recipe)
            el.barf(fname, lf("Book: {book}\nRecipe: {recipe}\n"))
            tee = subprocess.Popen(["setsid", "-w", "tee", "-a", fname], stdin=subprocess.PIPE)
            os.dup2(tee.stdin.fileno(), sys.stdout.fileno())
            os.dup2(tee.stdin.fileno(), sys.stderr.fileno())

        old_sc_hookfn = el.sc_hookfn
        log = CommandLog()
        el.sc_hookfn = log.record
        try:
            if "-p" in flags:
                sys.stdout = open(os.devnull, "w")
            ret_cmds = fun(42, *recipe_args(fun, args[1:])) or []
        except:
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
            if len(log.cmds) < 10:
                spec = inspect.getfullargspec(fun)
                if ("log" in spec.args) and spec.defaults and spec.defaults[spec.args.index("log") - 1] is None:
                    pass
                else:
                    print("\n".join(log.cmds))
            if ret_cmds:
                el.bash(ret_cmds, echo=True)

def modules(full=False, match=False):
    cook_dir = el.file_name_directory(recipes.__file__)
    cook_modules = el.directory_files(cook_dir, full, match)
    user_dir = el.expand_file_name("~/.cook.d")
    if el.file_exists_p(user_dir):
        df = el.directory_files(user_dir, full, match)
        user_modules = [
            f for f in df
            if os.path.isfile(el.expand_file_name(f, user_dir))
            and os.path.splitext(f)[1] == ".py"]
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
    args_missing = args_req[1 + len(args_provided):]
    if spec.defaults:
        args_missing = args_missing[:-len(spec.defaults)]
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

def complete(argv=None):
    if argv is None:
        argv = sys.argv
    assert argv[1] == "cook"
    (_flags, args) = parse_flags(argv[1:])

    # below, assume we're completing the last word
    # current word being completed is sys.argv[-1]
    args = args[:-1]

    # fix the difference between bash-completion.el and the actual bash completion
    if re.match(":.", args[0]):
        args = [":", args[0][1:]] + args[1:]

    if len(args) == 1:
        if args[0] == ":":
            print("\n".join(module_names()))
        else:
            rs = el.sc("cook --list").split("\n")
            # remove extra args
            rs = [re.split(" :", s)[0] for s in rs]
            fr = [r for r in rs if re.match(args[0], r)]
            print("\n".join(fr))
    elif len(args) == 2 and args[0] == ":":
        matching_cands = el.re_filter("^" + args[1], module_names())
        print("\n".join(matching_cands))
    elif len(args) == 3 and args[0] == ":":
        mod = get_module(args[1])
        cands = list(recipe_dict(mod).keys())
        matching_cands = el.re_filter("^" + args[2], cands)
        print("\n".join(matching_cands))
    elif len(args) >= 4 and args[0] == ":":
        mod = get_module(args[1])
        fun = recipe_dict(mod)[args[2]]
        part = args[-1]
        fun_args = function_arglist(fun)
        arg_idx = len(args) - 3
        if arg_idx < len(fun_args) and fun_args[arg_idx] in ["fname", "fnames"]:
            print(el.sc("compgen -f -- {part}"))
        else:
            args = [""]*recipe_arity(fun)
            print(fun(("complete", part), *args))
    else:
        print("")
