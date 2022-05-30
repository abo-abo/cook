def best_editor(recipe, editor):
    if type(recipe) == int:
        print(f"{editor} is the best editor!")
    elif recipe[0] == "complete":
        return ["ed", "emacs", "vi"]
