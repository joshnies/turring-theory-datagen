from tqdm import tqdm

from theory_data_gen.common import gen_item, gen_mask_token
from theory_data_gen.common.java import JAVA_PRIM_TYPES


def __gen_catch_struct_item(t=None):
    """Generate "catch" structure item."""

    # Generate mask tokens
    m_type = gen_mask_token(0) if t is None else t
    base_m_idx = 0 if t is None else -1

    m_var_name = gen_mask_token(base_m_idx + 1)

    # Generate "catch" structure pair
    return gen_item(
        f'catch ({m_type} {m_var_name}) {{',
        f'catch ({m_var_name}) {{'
    )


def gen_catch_blocks(write):
    """Generate all try-catch block data."""

    types = JAVA_PRIM_TYPES.copy()
    types.append(gen_mask_token(0))

    # Generate catch structures
    for t in tqdm(types, desc='Generating "catch" structures'):
        write(__gen_catch_struct_item(t))
