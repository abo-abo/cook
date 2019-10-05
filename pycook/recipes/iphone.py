#* Imports
import shlex

#* Recipes
def vlc_upload(recipe, fname):
    """VLC / Network / Sharing via WiFi."""
    return [
        "curl -F files[]=@" + shlex.quote(fname) + " http://192.168.178.12/upload.json"]
