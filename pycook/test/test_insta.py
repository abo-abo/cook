import tempfile
import os
from pycook import insta


def test_echo_with_string_interpolation():
    """Test that echo handles Python string interpolation patterns like {foo}."""
    with tempfile.TemporaryDirectory() as tmpdir:
        test_file = os.path.join(tmpdir, "test.txt")
        content = "Hello {world} and {foo}"

        result = insta.echo(content, test_file)

        assert result is True
        assert os.path.exists(test_file)
        with open(test_file) as f:
            assert f.read() == content


def test_echo_with_quotes():
    """Test that echo handles single and double quotes."""
    with tempfile.TemporaryDirectory() as tmpdir:
        test_file = os.path.join(tmpdir, "test.txt")
        content = "Hello 'single' and \"double\" quotes"

        result = insta.echo(content, test_file)

        assert result is True
        assert os.path.exists(test_file)
        with open(test_file) as f:
            assert f.read() == content
