def extract_page(recipe, fname, page):
    (base, ext) = fname.split(".")
    return f"pdftk {fname} cat {page} output {base}-{page}.{ext}"
