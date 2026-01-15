from pycook.cook import recipe_args_description, recipe_args
import tempfile
import os
import io
import sys
from datetime import datetime

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


def test_expand_tee_location():
    """Test that date placeholders in tee location are expanded."""
    from pycook.cook import expand_tee_location
    from unittest.mock import patch

    mock_date = datetime(2025, 3, 15, 10, 30, 0)
    with patch("pycook.cook.datetime") as mock_datetime:
        mock_datetime.now.return_value = mock_date
        result = expand_tee_location("~/logs/%Y/%m/%Y-%m-%d")

    assert result == os.path.expanduser("~") + "/logs/2025/03/2025-03-15"


def test_book_config_matches_stem():
    """Test that book_config matches config keys by book stem, not full path."""
    from pycook.cook import book_config
    from unittest.mock import patch, MagicMock

    mock_mod = MagicMock()
    mock_mod.config = {
        "gql": {"tee": {"location": "/gql/logs"}},
        "*": {"tee": {"location": "/default/logs"}}
    }

    with patch("pycook.cook.el.file_exists_p", return_value=True), \
         patch("pycook.cook.load_module", return_value=mock_mod):
        assert book_config("/home/user/.cook.d/gql.py") == {"tee": {"location": "/gql/logs"}}
        assert book_config("/other/path/foo.py") == {"tee": {"location": "/default/logs"}}
