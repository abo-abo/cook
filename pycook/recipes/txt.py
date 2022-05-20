def clean_up_term_escapes(recipe, fname):
    return f"sed -i 's/\x1b\\[[0-9;]*m//g' '{fname}'"
