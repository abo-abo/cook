import pycook.insta as st

def heic_to_jpg(recipe, fname_heic):
    fname_heic = "/home/oleh/Downloads/IMG_0794.HEIC"
    fname_jpg = fname_heic.replace("HEIC", "jpg")
    fname_jpg_50 = fname_jpg.replace(".jpg", "_50.jpg")
    st.bash([
        f"heif-convert -q 85 {fname_heic} {fname_jpg}",
        f"convert {fname_jpg} -resize 50% {fname_jpg_50}"])
