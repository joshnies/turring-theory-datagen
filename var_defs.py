import random

from tqdm import tqdm
from constants import AI_VAR_NAME, AI_VAL, AI_USER_TYPE
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
    for t in tqdm(CPP_PRIM_TYPES, desc='Generating variable definitions (primitive types)'):
        # No default value
        (source, target) = gen_var_def_pair(t, has_default_value=False)
        item = {'source': source, 'target': target}
        data.append(item)

        # With default value
        (source, target) = gen_var_def_pair(t, has_default_value=True)
        item = {'source': source, 'target': target}
        data.append(item)

    # Generate user type variable definition pair
    (source, target) = gen_var_def_pair(AI_USER_TYPE, has_default_value=False)
    item = {'source': source, 'target': target}
    data.append(item)

    (source, target) = gen_var_def_pair(AI_USER_TYPE, has_default_value=True)
    item = {'source': source, 'target': target}
    data.append(item)

    # Generate generic variable definition pairs
    for _ in tqdm(range(generic_count), desc='Generating variable definitions (generic types)'):
        t = gen_cpp_generic_type()
        (source, target) = gen_var_def_pair(t, has_default_value=bool(random.getrandbits(1)))
        item = {'source': source, 'target': target}
        data.append(item)

    return data
