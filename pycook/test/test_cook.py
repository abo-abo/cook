from pycook.cook import recipe_args_description, recipe_args

def local_1(recipe, db=["mysql", "postgres", "sqlite"]):
    return db + " $DATABASE_URL"

def local_2(recipe, db=["mysql", "postgres", "sqlite"], config={"pty": True}):
    return db + " $DATABASE_URL"

def test_recipe_args_description_1():
    assert recipe_args_description(local_1) == " :db=(mysql|postgres|sqlite)"

def test_recipe_args_description_2():
    assert recipe_args_description(local_2) == " :db=(mysql|postgres|sqlite)"

def test_recipe_args():
    args = recipe_args(local_1, [":db", "postgres"])
    assert args == ["postgres"]
