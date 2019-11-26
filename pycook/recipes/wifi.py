#* Imports
import shlex
import pycook.elisp as el

#* Recipes
def show_current_password(recipe):
    this_connection = el.sc_l("nmcli -t connection")[0]
    (name, *_) = this_connection.split(":")
    return [
        "sudo cat /etc/NetworkManager/system-connections/" + shlex.quote(name)
    ]
