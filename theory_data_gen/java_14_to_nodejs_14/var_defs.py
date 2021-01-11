from tqdm import tqdm

from theory_data_gen.common import gen_item, gen_mask_token, add_mask_indices
from theory_data_gen.constants import MASK_TOKEN
from theory_data_gen.common.java import JAVA_PRIM_TYPES, gen_java_generic_type
from .java import gen_modifier_permutations


def __gen_var_def_items(t: str):
    """Generate variable definition items (no default value)."""

    src, name_mask_index = add_mask_indices(f'{t} {MASK_TOKEN};')
    tar = f'let {gen_mask_token(name_mask_index)};'

    return gen_modifier_permutations(gen_item(src, tar), include_abstract=False)


def gen_var_defs(write, array_count: int):
    """Generate variable definitions with no default value."""

    types = JAVA_PRIM_TYPES.copy()
    types.append(MASK_TOKEN)

    # Standard
    for t in tqdm(types, desc='Generating variable definitions (no default values)'):
        for i in __gen_var_def_items(t):
            write(i)

    # Generic
    for _ in tqdm(range(array_count), desc='Generating array variable definitions (no default value)'):
        for i in __gen_var_def_items(gen_java_generic_type()):
            write(i)
