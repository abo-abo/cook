def random(recipe, n="12"):
    return f"tr -dc A-Za-z0-9 </dev/urandom | head -c {n} ; echo ''"
