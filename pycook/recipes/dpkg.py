import pycook.elisp as el

def ls(recipe, package):
    if type(recipe) is int:
        return ["dpkg -L " + package]
    elif recipe[0] == "complete":
        part = recipe[1]
        if len(part) > 0:
            part += "*"
        return el.shell_command_to_string("dpkg-query -W -f '${Package}\n' " + part)


def file_to_package(recipe, fname):
    return ["dpkg -S " + fname]
