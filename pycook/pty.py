import os
import re
from pycook import elisp as el
from invoke import Local, Context


class CookLocal(Local):
    def __init__(self, context, cmd):
        Local.__init__(self, context)
        history_dir = os.path.expanduser("~/.cook.d/history/")
        el.make_directory(history_dir)
        self.history_fname = history_dir + cmd
        if el.file_exists_p(self.history_fname):
            with open(self.history_fname, "r") as fh:
                history_txt = fh.read()
            self.history = el.delete_dups(
                el.delete("", re.split(r"\n?--\n?", history_txt))
            )
        else:
            self.history = []
        self.stdin_echo = open(self.history_fname, "w")
        print(
            "\n--\n".join(self.history) + "\n--",
            file=self.stdin_echo,
            flush=True,
            end="",
        )

    def read_our_stdin(self, input_):
        r = Local.read_our_stdin(self, input_)
        if r:
            hitem = r.strip()
            if hitem not in self.history:
                self.history.append(hitem)
                print("\n--\n" + hitem, file=self.stdin_echo, flush=True, end="")
        return r


def make_runner(cmd: str) -> CookLocal:
    return CookLocal(Context(), cmd)
