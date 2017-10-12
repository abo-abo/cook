#* Recipes
def files_in_dir(recipe):
    return ["ls"]

def files_in_parent_dir(recipe):
    res = ["cd .."]
    res += ["find ."]
    return res

def last_commit(recipe):
    return ["git rev-parse HEAD"]
