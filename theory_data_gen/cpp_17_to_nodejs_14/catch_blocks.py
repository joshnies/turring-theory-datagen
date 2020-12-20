from tqdm import tqdm

from theory_data_gen.common import gen_item, gen_mask_token
from .cpp import CPP_PRIM_TYPES


def __gen_catch_struct_pair(t=None):
    """Generate "catch" structure pair."""

    # Generate mask tokens
    m_type = gen_mask_token(0) if t is None else t
    base_m_idx = 0 if t is None else -1

    m_var_name = gen_mask_token(base_m_idx + 1)

    # Generate "catch" structure pair
    source = f'catch ({m_type} {m_var_name}) {{'
    target = f'catch ({m_var_name}) {{'
    return source, target


def gen_catch_blocks():
    """Generate all try-catch block data."""

    types = CPP_PRIM_TYPES.copy()
    types.append(gen_mask_token(0))
    data = list()

    # Generate catch structures
    for t in tqdm(types, desc='Generating "catch" structures'):
        (source, target) = __gen_catch_struct_pair(t)
        data.append(gen_item(source, target))

    return data
