def ls(recipe):
    package = input("package: ")
    return ["dpkg -L " + package]
