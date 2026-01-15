from pycook.cook import completions
from pytest_subprocess import FakeProcess


def test_completions(fp: FakeProcess):
    fp.register(["bash", "-c", "apt-cache pkgnames python3-num"], stdout="python3-numpy\npython3-numba")
    comps = completions(["cook", ":", "apt", "install", "python3-num", "4"])
    assert "python3-numpy" in comps
    assert "emacs" in completions(["cook", ":", "examples", "best_editor", "", "4"])
    assert "vi" == completions(["cook", ":", "examples", "best_editor", "v", "4"])
    assert completions(['cook', ':', 'net', '', '3'])
