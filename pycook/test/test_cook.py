from pycook.cook import recipe_args_description, recipe_args

def local(recipe, config={"select_db": ["mysql", "postgres", "sqlite"]}):
    return config["select_db"] + " $DATABASE_URL"


def test_recipe_args_description():
    assert recipe_args_description(local) == " :db=(mysql|postgres|sqlite)"

def test_recipe_args():
    args = recipe_args(local, [":db", "postgres"])
    assert args == [{"select_db": "postgres"}]
