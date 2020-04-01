def enddate(recipe, fname):
    return "openssl x509 -enddate -noout -in " + fname

def txt(recipe, fname):
    return "openssl x509 -text -in " + fname
