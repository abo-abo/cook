def ls(recipe):
    package = input("package: ")
    return ["dpkg -L " + package]

def file_to_package(recipe, fname):
    return ["dpkg -S " + fname]
