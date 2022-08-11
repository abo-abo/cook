from pycook.cook import recipe_args_description

def local(recipe, config={"select_db": ["mysql", "postgres", "sqlite"]}):
    pass

def test_recipe_args_description():
    assert recipe_args_description(local) == " :db=(mysql|postgres|sqlite)"
