from pycook.cook import completions


def test_completions():
    comps = completions(["cook", ":", "apt", "install", "python3-num", "4"])
    assert "python3-numpy" in comps
    assert "emacs" in completions(["cook", ":", "examples", "best_editor", "", "4"])
    assert "vi" == completions(["cook", ":", "examples", "best_editor", "v", "4"])
    assert completions(['cook', ':', 'net', '', '3'])
