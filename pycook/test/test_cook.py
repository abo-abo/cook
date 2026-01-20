from pycook.cook import recipe_args_description, recipe_args, module_names, get_module, extract_cd_from_ast
import tempfile
import os
import io
import sys
from datetime import datetime
from unittest.mock import patch

def local_1(recipe, db=["mysql", "postgres", "sqlite"]):
    return db + " $DATABASE_URL"

def local_2(recipe, db=["mysql", "postgres", "sqlite"], config={"pty": True}):
    return db + " $DATABASE_URL"

def test_recipe_args_description_1():
    assert recipe_args_description(local_1) == " :db=(mysql|postgres|sqlite)"

def test_recipe_args_description_2():
    assert recipe_args_description(local_2) == " :db=(mysql|postgres|sqlite) :config :pty=True"

def test_recipe_args():
    args = recipe_args(local_1, [":db", "postgres"])
    assert args == ["postgres"]


def test_tee_capture():
    """Test that tee captures stdout and writes to a log file."""
    from pycook import cook

    with tempfile.TemporaryDirectory() as tmpdir:
        # Create a test Cookbook
        cookbook_path = os.path.join(tmpdir, "Cookbook.py")
        with open(cookbook_path, "w") as f:
            f.write("""
def print_recipe(recipe):
    print("Hello from tee test")
    return []
""")

        tee_dir = os.path.join(tmpdir, "logs")
        os.makedirs(tee_dir)

        # Mock book_config to return tee configuration
        original_book_config = cook.book_config
        cook.book_config = lambda book: {"tee": {"location": tee_dir}}

        # Capture stdout to verify tee also prints
        captured = io.StringIO()
        original_stdout = sys.stdout

        try:
            sys.stdout = captured
            cook._main(cookbook_path, "test", "", ["print_recipe"])
            sys.stdout = original_stdout

            # Verify output was printed (tee should print captured output)
            assert "Hello from tee test" in captured.getvalue()

            # Verify log file was created
            log_files = []
            for root, dirs, files in os.walk(tee_dir):
                log_files.extend(files)
            assert len(log_files) == 1

            # Verify log file content
            log_path = os.path.join(tee_dir, log_files[0])
            # Find the actual file path (may be in subdirectory)
            for root, dirs, files in os.walk(tee_dir):
                if files:
                    log_path = os.path.join(root, files[0])
                    break
            with open(log_path) as f:
                content = f.read()
            assert "Hello from tee test" in content
            assert "print_recipe" in content

        finally:
            sys.stdout = original_stdout
            cook.book_config = original_book_config


def test_log_file_name_with_date_placeholders():
    """Test that date placeholders in location are expanded."""
    from pycook.cook import log_file_name
    from unittest.mock import patch

    user_dir = os.path.expanduser("~/.cook.d")
    mock_date = datetime(2025, 3, 15, 10, 30, 0)

    def mock_expand(p, base=None):
        if base:
            return os.path.join(base, p)
        return os.path.expanduser(p)

    with (
        patch("pycook.cook.datetime") as mock_datetime,
        patch("pycook.cook.el.make_directory"),
        patch("pycook.cook.el.expand_file_name", side_effect=mock_expand),
    ):
        mock_datetime.now.return_value = mock_date
        # Top-level module
        result = log_file_name("~/logs/%Y/%m/%d", f"{user_dir}/gql.py", "my_recipe")
        assert result == os.path.expanduser("~") + "/logs/2025/03/15/10:30_cook:gql:my_recipe.txt"
        # Nested module includes parent package in name
        result = log_file_name("~/logs/%Y/%m/%d", f"{user_dir}/gql/support.py", "my_recipe")
        assert result == os.path.expanduser("~") + "/logs/2025/03/15/10:30_cook:gql.support:my_recipe.txt"


def test_book_config_matches_stem():
    """Test that book_config matches config keys by book stem, not full path."""
    from pycook.cook import book_config
    from unittest.mock import patch, MagicMock
    import os

    mock_mod = MagicMock()
    mock_mod.config = {
        "gql": {"tee": {"location": "/gql/logs"}},
        "gql.support": {"tee": {"location": "/gql-support/logs"}},
        "*": {"tee": {"location": "/default/logs"}}
    }

    user_dir = os.path.expanduser("~/.cook.d")

    with (
        patch("pycook.cook.el.file_exists_p", return_value=True),
        patch("pycook.cook.load_module", return_value=mock_mod),
        patch("pycook.cook.el.expand_file_name", side_effect=os.path.expanduser),
    ):
        # Top-level module matches by stem
        assert book_config(f"{user_dir}/gql.py") == {"tee": {"location": "/gql/logs"}}
        # Nested module with exact match
        assert book_config(f"{user_dir}/gql/support.py") == {"tee": {"location": "/gql-support/logs"}}
        # Nested module falls back to parent package config
        assert book_config(f"{user_dir}/gql/merchant.py") == {"tee": {"location": "/gql/logs"}}
        # Unknown module falls back to wildcard
        assert book_config("/other/path/foo.py") == {"tee": {"location": "/default/logs"}}


def test_module_names_includes_nested_modules():
    """Test that module_names() includes nested modules from subdirectories."""
    with tempfile.TemporaryDirectory() as tmpdir:
        # Create a nested module structure
        nested_dir = os.path.join(tmpdir, "gql")
        os.makedirs(nested_dir)

        # Create module files
        with open(os.path.join(nested_dir, "__init__.py"), "w") as f:
            f.write("")
        with open(os.path.join(nested_dir, "support.py"), "w") as f:
            f.write("def test_recipe(recipe): pass")
        with open(os.path.join(nested_dir, "merchant.py"), "w") as f:
            f.write("def test_recipe(recipe): pass")
        # Create a top-level module too
        with open(os.path.join(tmpdir, "toplevel.py"), "w") as f:
            f.write("def test_recipe(recipe): pass")

        def mock_expand(p, base=None):
            if p == "~/.cook.d":
                return tmpdir
            if base:
                return os.path.join(base, p)
            return os.path.expanduser(p)

        with patch("pycook.cook.el.expand_file_name", side_effect=mock_expand), \
             patch("pycook.cook.el.file_exists_p", return_value=True), \
             patch("pycook.cook.el.directory_files", return_value=["toplevel.py"]):
            names = module_names()
            assert "gql.support" in names
            assert "gql.merchant" in names


def test_get_module_handles_dotted_names():
    """Test that get_module() handles dotted module names like 'gql.support'."""
    with tempfile.TemporaryDirectory() as tmpdir:
        # Create a nested module structure
        nested_dir = os.path.join(tmpdir, "gql")
        os.makedirs(nested_dir)

        support_path = os.path.join(nested_dir, "support.py")
        with open(support_path, "w") as f:
            f.write("def test_recipe(recipe): pass")

        def mock_expand(p):
            if p == "~/.cook.d":
                return tmpdir
            return os.path.expanduser(p)

        with (
            patch("pycook.cook.el.expand_file_name", side_effect=mock_expand),
            patch("pycook.cook.el.file_exists_p", side_effect=os.path.exists),
        ):
            result = get_module("gql.support")
            assert result == support_path


def test_get_module_nested_not_found_raises():
    """Test that get_module() raises RuntimeError for non-existent nested modules."""
    with tempfile.TemporaryDirectory() as tmpdir:

        def mock_expand(p):
            if p == "~/.cook.d":
                return tmpdir
            return os.path.expanduser(p)

        with (
            patch("pycook.cook.el.expand_file_name", side_effect=mock_expand),
            patch("pycook.cook.el.file_exists_p", return_value=False),
        ):
            try:
                get_module("nonexistent.module")
                assert False, "Should have raised RuntimeError"
            except RuntimeError as e:
                assert "Module not found" in str(e)


def test_extract_cd_from_ast():
    """Test that extract_cd_from_ast extracts cd path from recipe return statements."""
    with tempfile.TemporaryDirectory() as tmpdir:
        cookbook_path = os.path.join(tmpdir, "Cookbook.py")
        with open(cookbook_path, "w") as f:
            f.write("""
def with_cd(recipe):
    return [
        "cd ~/my/project",
        "echo hello"]

def no_cd(recipe):
    return ["echo hello"]

def empty_return(recipe):
    return []

def no_return(recipe):
    print("hello")

def cd_not_first(recipe):
    return [
        "echo setup",
        "cd ~/somewhere"]
""")

        # Recipe with cd as first element
        assert extract_cd_from_ast(cookbook_path, "with_cd") == "~/my/project"

        # Recipe without cd
        assert extract_cd_from_ast(cookbook_path, "no_cd") is None

        # Recipe with empty return
        assert extract_cd_from_ast(cookbook_path, "empty_return") is None

        # Recipe with no return statement
        assert extract_cd_from_ast(cookbook_path, "no_return") is None

        # Recipe where cd is not the first element (should not match)
        assert extract_cd_from_ast(cookbook_path, "cd_not_first") is None

        # Non-existent function
        assert extract_cd_from_ast(cookbook_path, "nonexistent") is None
