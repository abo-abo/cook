def ls_users(recipe):
    return "cut -d: -f1 /etc/group | sort"

def ls_groups(recipe):
    return "cut -d: -f1 /etc/group | sort"

def ls_cpus(recipe):
    return "cat /proc/cpuinfo | awk '/^processor/{print $3}'"
