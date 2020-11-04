from constants import AI_VAR_NAME
from cpp import CPP_PRIM_TYPES


def gen_var_def_pair(t: str):
    """Generate a variable pair."""

    source = f'{t} {AI_VAR_NAME};'
    target = f'let {AI_VAR_NAME};'
    return source, target


def gen_var_defs():
    """Generate all variable definition data."""

    data = []

    for t in CPP_PRIM_TYPES:
        (source, target) = gen_var_def_pair(t)
        item = {'source': source, 'target': target}
        data.append(item)

    return data
