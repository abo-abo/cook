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
    rc_file = el.expand_file_name("~/.cookrc.py")
    if el.file_exists_p(rc_file):
        mod = imp.load_source("book_config", rc_file)
        config = mod.config
        if book in config:
            return config[book]
        elif "*" in config:
            return config["*"]
    return {}

def recipe_args_description(f):
    arity = recipe_arity(f)
    if arity == 0:
        return ""
    else:
        return " " + " ".join([":" + a for a in function_arglist(f)[1:]])

def recipe_names(book):
    di = recipe_dict(book)
    ns = [k + recipe_args_description(v) for (k, v) in di.items()]
    return "\n".join(ns)

def describe(book, module=""):
    res = "usage: cook"
    if module:
        res += " :" + module
    res += " <recipe>\n"
    res += book + "\n"
    return res + "Available recipes:\n" + recipe_names(book)

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
    return len(function_arglist(f)) - 1

def _main(argv, book):
    if len(argv) == 2:
        if argv[1] == "--list":
            print(recipe_names(book))
        elif argv[1] == "--help":
            print(describe(book))
        else:
            recipe = argv[1]
            fun = recipe_dict(book)[recipe]
            cfg = book_config(book)
            if "tee" in cfg and recipe != "bash":
                basedir = cfg["tee"]["location"]
                fname = log_file_name(basedir, book, recipe)
                el.barf(fname, lf("Book: {book}\nRecipe: {recipe}\n"))
                tee = subprocess.Popen(["tee", "-a", fname], stdin=subprocess.PIPE)
                os.dup2(tee.stdin.fileno(), sys.stdout.fileno())
                os.dup2(tee.stdin.fileno(), sys.stderr.fileno())

            cmds = []
            old_sc_hookfn = el.sc_hookfn
            el.sc_hookfn = lambda s: cmds.append("# " + re.sub("\n", "\\\\n", s))
            try:
                ret_cmds = fun(42) or []
            except:
                if cfg.get("pdb", False):
                    import pdb
                    pdb.set_trace()
                    return
                else:
                    raise
            print("\n".join(cmds))
            all_cmds = ret_cmds
            el.sc_hookfn = old_sc_hookfn
            el.bash(all_cmds, echo=True)
    elif len(argv) >= 2 and recipe_arity(recipe_dict(book)[argv[1]]) == len(argv[2:]):
        recipe = argv[1]
        fun = recipe_dict(book)[recipe]
        el.bash(fun(42, *argv[2:]) or [], echo=True)
    else:
        print(describe(book))

def modules(full=False, match=False):
    cook_dir = el.file_name_directory(recipes.__file__)
    cook_modules = el.directory_files(cook_dir, full, match)
    cook_modules = list(filter(lambda s: not re.search("__", s), cook_modules))
    user_dir = el.expand_file_name("~/.cook.d")
    if el.file_exists_p(user_dir):
        df = el.directory_files(user_dir, full, match)
        user_modules = [
            f for f in df
            if os.path.isfile(el.expand_file_name(f, user_dir))
            and os.path.splitext(f)[1] == ".py"]
    else:
        user_modules = []
    return cook_modules + user_modules

def module_names():
    ms = modules(False, "[^_]\\.py$")
    return el.delete_dups([s[:-3] for s in ms])

def recipe_args(f, args_provided):
    spec = inspect.getfullargspec(f)
    args_req = spec.args
    assert args_req[0] == "recipe"
    args_missing = args_req[1 + len(args_provided):]
    args_input = []
    for a in args_missing:
        args_input.append(input(a + ": "))
    return args_provided + args_input

def main(argv=None):
    if argv is None:
        argv = sys.argv
    try:
        if len(argv) >= 2 and re.match("^:", argv[1]):
            module = argv[1][1:]
            if module == "":
                for module in modules(True):
                    print(module)
                sys.exit(0)
            book = get_module(module)
            if len(argv) >= 3:
                recipe = argv[2]
                if recipe == "--list":
                    print(recipe_names(book))
                else:
                    fun = recipe_dict(book)[recipe]
                    cmds = fun(42, *recipe_args(fun, argv[3:])) or []
                    el.bash(cmds)
                    sys.exit(0)
            else:
                print(describe(book, module))
        else:
            (book, dd) = script_get_book()
            os.chdir(dd)
            _main(argv, book)
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
    # below, assume we're completing the last word
    # current word being completed is sys.argv[-1]
    if len(argv) == 3:
        args = argv[2:]
    else:
        args = argv[2:-1]

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
        if len(fun_args) == 2 and fun_args[1] in ["fname", "fnames"]:
            print(el.sc("compgen -f -- {part}"))
        else:
            args = [""]*recipe_arity(fun)
            print(fun(("complete", part), *args))
    else:
        print("")
