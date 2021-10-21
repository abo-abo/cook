#* Imports
import pycook.insta as st

#* Recipes
def install(recipe, package, version=None):
    st.install_package_version(package, version)
