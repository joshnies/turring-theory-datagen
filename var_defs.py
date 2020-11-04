import random
from constants import AI_VAR_NAME, AI_VAL
from cpp import CPP_PRIM_TYPES, gen_cpp_generic_type


def gen_var_def_pair(t: str, has_default_value: bool):
    """Generate a variable definition pair."""

    default_value = f' = {AI_VAL}' if has_default_value else ''

    source = f'{t} {AI_VAR_NAME}{default_value};'
    target = f'let {AI_VAR_NAME}{default_value};'
    return source, target


def gen_var_defs(generic_count):
    """Generate all variable definition data."""

    data = []

    # Generate variable definition pairs for pre-defined types
    for t in CPP_PRIM_TYPES:
        # No default value
        (source, target) = gen_var_def_pair(t, has_default_value=False)
        item = {'source': source, 'target': target}
        data.append(item)

        # With default value
        (source, target) = gen_var_def_pair(t, has_default_value=True)
        item = {'source': source, 'target': target}
        data.append(item)

    # Generate generic variable definition pairs
    for i in range(generic_count):
        t = gen_cpp_generic_type()
        (source, target) = gen_var_def_pair(t, has_default_value=bool(random.getrandbits(1)))
        item = {'source': source, 'target': target}
        data.append(item)

    return data
