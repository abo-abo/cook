#* Imports
import re
import shlex
import pycook.insta as st
import pycook.elisp as el

#* Functions
def this_ip():
    out = el.sc("ip route get 1.1.1.1")
    ms = re.findall("src ([0-9.]+)", out)
    assert len(ms) == 1
    return ms[0]

#* Recipes
def ip(recipe):
    print(this_ip())

def ip4(recipe):
    # install dnsutils on Debian
    res = st.run("dig +short myip.opendns.com @resolver1.opendns.com")
    res = res or st.run("dig -4 TXT +short o-o.myaddr.l.google.com @ns1.google.com").strip('"')
    print(res)

def ip6(recipe):
    print(st.run("dig TXT +short o-o.myaddr.l.google.com @ns1.google.com").strip('"'))

def show_wifi_password(recipe):
    this_connection = el.sc_l("nmcli -t connection")[0]
    (name, *_) = this_connection.split(":")
    return [
        "sudo cat /etc/NetworkManager/system-connections/" + shlex.quote(name)
    ]
