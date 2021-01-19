from tqdm import tqdm

from theory_data_gen.common import gen_item, gen_mask_token, add_mask_indices
from theory_data_gen.constants import MASK_TOKEN, MEMBER_TOKEN
from theory_data_gen.common.java import JAVA_PRIM_TYPES_W_MASK, gen_java_generic_type
from .java import gen_modifier_permutations, to_member_items


def __gen_var_def_items(t: str, member=False):
    """Generate variable definition items (no default value)."""

    member_prefix = f'{MEMBER_TOKEN} ' if member else ''
    js_decl = 'let ' if not member else ''

    src, name_mask_index = add_mask_indices(f'{member_prefix}{t} {MASK_TOKEN};')
    tar = f'{js_decl}{gen_mask_token(name_mask_index)};'

    items = gen_modifier_permutations(gen_item(src, tar), include_abstract=False)

    if member:
        items = to_member_items(items)

    return items


def gen_var_defs(write, array_count: int):
    """Generate variable definitions with no default value."""

    # Standard
    for t in tqdm(JAVA_PRIM_TYPES_W_MASK, desc='Generating variable definitions (no default values)'):
        items = __gen_var_def_items(t, member=False)
        items.extend(__gen_var_def_items(t, member=True))

        for i in items:
            write(i)

    # Generic
    for _ in tqdm(range(array_count), desc='Generating array variable definitions (no default value)'):
        t = gen_java_generic_type()
        items = __gen_var_def_items(t, member=False)
        items.extend(__gen_var_def_items(t, member=True))

        for i in items:
            write(i)
