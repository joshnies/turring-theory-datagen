from tqdm import tqdm

from theory_data_gen.common import gen_item, gen_mask_token, add_mask_indices
from theory_data_gen.constants import MASK_TOKEN
from .cpp import CPP_PRIM_TYPES, gen_cpp_generic_type


def __gen_var_def_item(t: str):
    """Generate variable definition item with no default value."""

    src, name_mask_index = add_mask_indices(f'{t} {MASK_TOKEN};')
    tar = f'let {gen_mask_token(name_mask_index)};'

    return gen_item(src, tar)


def gen_var_defs(array_count: int):
    """Generate variable definitions with no default value."""

    data = []

    types = CPP_PRIM_TYPES.copy()
    types.append(MASK_TOKEN)

    # Standard
    for t in tqdm(types, desc='Generating variable definitions (no default values)'):
        data.append(__gen_var_def_item(t))

    # Generic
    for _ in tqdm(range(array_count), desc='Generating array variable definitions (no default value)'):
        data.append(__gen_var_def_item(gen_cpp_generic_type()))

    return data
