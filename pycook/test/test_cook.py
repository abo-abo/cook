from pycook.cook import completions


def test_completions():
    assert "python3-numpy" in completions(["cook", ":", "apt", "install", "python3-num", "4"])
    assert "emacs" in completions(["cook", ":", "examples", "best_editor", "", "4"])
    assert "vi" == completions(["cook", ":", "examples", "best_editor", "v", "4"])
