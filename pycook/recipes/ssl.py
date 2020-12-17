#* Imports
import pycook.insta as st

#* Recipes
def enddate(recipe, fname):
    return "openssl x509 -enddate -noout -in " + fname

def txt(recipe, fname):
    return "openssl x509 -text -in " + fname

def pfx_to_pem(recipe, fname):
    """FNAME is a pfx file."""
    f_key = "privatekey.pem"
    f_cert = "publiccert.pem"
    f_pem = "intermediate.pem"
    f_ppem = "final.pem"
    st.make(f_key, f"openssl pkcs12 -in {fname} -nocerts -out {f_key} -nodes")
    st.make(f_cert, f"openssl pkcs12 -in {fname} -nokeys -out {f_cert} -nodes")
    st.make(f_pem, f"cat {f_key} {f_cert} > {f_pem}")
    st.make(f_ppem, [
        f"openssl rsa -in {f_pem} -out {f_ppem}",
        f"openssl x509 -in {f_pem} >> {f_ppem}"
    ])
