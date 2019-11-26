#* Imports
import re
import pycook.elisp as el
sc = el.sc

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
    res = el.sc("dig +short myip.opendns.com @resolver1.opendns.com")
    res = res or sc("dig -4 TXT +short o-o.myaddr.l.google.com @ns1.google.com").strip('"')
    print(res)

def ip6(recipe):
    print(sc("dig TXT +short o-o.myaddr.l.google.com @ns1.google.com").strip('"'))
