import random

from tqdm import tqdm

from theory_data_gen.common import gen_mask_token
from .cpp import CPP_PRIM_TYPES, gen_cpp_generic_type


def gen_var_def_pair(t=None, has_default_value=False):
    """Generate a variable definition pair."""

    # Generate mask tokens
    m_type = gen_mask_token(0) if t is None else t
    base_mask_idx = 0 if t is None else -1
    m_var_name = gen_mask_token(base_mask_idx + 1)

    # Generate var def pair
    default_value = f' = {gen_mask_token(base_mask_idx + 2)}' if has_default_value else ''

    source = f'{m_type} {m_var_name}{default_value};'
    target = f'let {m_var_name}{default_value};'
    return source, target


def gen_var_defs(generic_count):
    """Generate variable definitions."""

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
    (source, target) = gen_var_def_pair(has_default_value=False)
    item = {'source': source, 'target': target}
    data.append(item)

    (source, target) = gen_var_def_pair(has_default_value=True)
    item = {'source': source, 'target': target}
    data.append(item)

    # Generate generic variable definition pairs
    for _ in tqdm(range(generic_count), desc='Generating variable definitions (generic types)'):
        t = gen_cpp_generic_type()
        (source, target) = gen_var_def_pair(t, has_default_value=bool(random.getrandbits(1)))
        item = {'source': source, 'target': target}
        data.append(item)

    return data
