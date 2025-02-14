import subprocess
import shlex
from typing import Any


def run(
    cmds: list[str],
    capture_output: bool = True,
    **kwargs: Any,
) -> str:
    """Execute cmd and return stdout."""
    r = subprocess.run(
        cmds,
        capture_output=capture_output,
        encoding="utf-8",
        check=True,
        **kwargs,
    )
    if r.stdout:
        return r.stdout.strip()
    return ""


def sh(
    cmd: str,
    capture_output: bool = True,
    **kwargs: Any,
) -> str:
    """Execute cmd and return stdout."""
    return run(shlex.split(cmd))
