from pycook.cook import recipe_args_description, recipe_args
import tempfile
import os
import io
import sys

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
