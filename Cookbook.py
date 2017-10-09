#!/usr/bin/env python
import cook

def pip_cook_installed_p():
    res = cook.elisp.shell_command_to_string(
        "pip show cook || echo 'n'").strip()
    return res != "n"

def pip_reinstall(recipe):
    res = []
    if pip_cook_installed_p():
        res.append("sudo -H pip uninstall -y cook")
    res.append("sudo -H pip install .")
    return res

if __name__ == '__main__':
    cook.main()
