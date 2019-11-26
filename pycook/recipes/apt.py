import pycook.elisp as el
import pycook.insta as st

def install(recipe, package=None):
    if recipe == 42:
        st.install_package(package)
    elif recipe[0] == "complete":
        return el.sc("apt-cache pkgnames " + recipe[1])

def history(recipe):
    return [
        """(zcat $(ls -tr /var/log/apt/history.log*.gz); cat /var/log/apt/history.log) 2>/dev/null |
            egrep '^(Start-Date:|Commandline:)' |
            grep -v aptdaemon |
            egrep '^Commandline:' | awk '!x[$0]++'
        """
    ]

def remove(recipe, package=None):
    if recipe == 42:
        return [el.lf("sudo apt-get remove {package}")]
    elif recipe[0] == "complete":
        return el.shell_command_to_string(
            "dpkg --get-selections " + recipe[1] + "* | awk '{print $1}'")
