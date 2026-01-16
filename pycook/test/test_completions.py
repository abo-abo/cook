from pycook.cook import completions, module_names
from pytest_subprocess import FakeProcess
from unittest.mock import patch
import tempfile
import os


def test_completions(fp: FakeProcess):
    fp.register(
        ["bash", "-c", "apt-cache pkgnames python3-num"],
        stdout="python3-numpy\npython3-numba",
    )
    comps = completions(["cook", ":", "apt", "install", "python3-num", "4"])
    assert "python3-numpy" in comps
    assert "emacs" in completions(["cook", ":", "examples", "best_editor", "", "4"])
    assert "vi" == completions(["cook", ":", "examples", "best_editor", "v", "4"])
    assert completions(["cook", ":", "net", "", "3"])


def test_completions_nested_module_names():
    """Test that completion lists nested module names like gql.support."""
    with tempfile.TemporaryDirectory() as tmpdir:
        # Create nested module structure
        nested_dir = os.path.join(tmpdir, "gql")
        os.makedirs(nested_dir)
        with open(os.path.join(nested_dir, "__init__.py"), "w") as f:
            f.write("")
        with open(os.path.join(nested_dir, "support.py"), "w") as f:
            f.write("def my_recipe(recipe): pass")
        with open(os.path.join(nested_dir, "merchant.py"), "w") as f:
            f.write("def other_recipe(recipe): pass")

        def mock_expand(p, base=None):
            if p == "~/.cook.d":
                return tmpdir
            if base:
                return os.path.join(base, p)
            return os.path.expanduser(p)

        with (
            patch("pycook.cook.el.expand_file_name", side_effect=mock_expand),
            patch("pycook.cook.el.file_exists_p", return_value=True),
            patch("pycook.cook.el.directory_files", return_value=[]),
        ):
            names = module_names()
            assert "gql.support" in names
            assert "gql.merchant" in names

            # Test completion filtering
            comps = completions(["cook", ":", "gql.", "3"])
            assert "gql.support" in comps
            assert "gql.merchant" in comps


def test_completions_nested_module_recipes():
    """Test that completion lists recipes from nested modules."""
    with tempfile.TemporaryDirectory() as tmpdir:
        # Create nested module structure
        nested_dir = os.path.join(tmpdir, "testmod")
        os.makedirs(nested_dir)
        with open(os.path.join(nested_dir, "__init__.py"), "w") as f:
            f.write("")
        with open(os.path.join(nested_dir, "sub.py"), "w") as f:
            f.write("def recipe_one(recipe): pass\ndef recipe_two(recipe): pass")

        def mock_expand(p):
            if p == "~/.cook.d":
                return tmpdir
            return os.path.expanduser(p)

        with patch("pycook.cook.el.expand_file_name", side_effect=mock_expand):
            comps = completions(["cook", ":", "testmod.sub", "", "4"])
            assert "recipe_one" in comps
            assert "recipe_two" in comps
