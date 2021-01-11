import random

from tqdm import tqdm

from theory_data_gen.common import add_mask_indices, add_scope_open_token, gen_item, gen_mask_token, gen_type_generics
from theory_data_gen.common.java import gen_inheritance
from theory_data_gen.constants import MASK_TOKEN
from .java import gen_modifier_permutations


def gen_interface_items():
    """Generate interface items."""

    # Generate generics
    generics = gen_type_generics()

    # Generate inheritance for interface
    inheritance_range = range(0, 11)
    inheritance_count = \
        random.choices(inheritance_range, weights=(70, 20, 15, 10, 7, 6, 5, 4, 3, 2, 1), k=1)[0]
    inheritance = gen_inheritance(inheritance_count) if inheritance_count > 0 else ''

    # Generate base item
    src_base_wo_inher = f'interface {MASK_TOKEN}{generics}'
    _, tar_start_idx = add_mask_indices(src_base_wo_inher)
    tar_start_idx += 1

    src_base = f'{src_base_wo_inher}{inheritance}'
    tar_base = f'class {gen_mask_token(0)}{inheritance}'

    src_base, _ = add_mask_indices(src_base)
    tar_base, _ = add_mask_indices(tar_base, start_index=tar_start_idx)
    item_base = gen_item(src_base, tar_base)

    items = gen_modifier_permutations(item_base, include_static=False, include_abstract=False)
    items.extend(
        list(map(lambda i: add_scope_open_token(i), items))
    )

    return items


def gen_interfaces(write, count):
    """Generate interfaces."""

    for _ in tqdm(range(count), desc='Generating interfaces'):
        for i in gen_interface_items():
            write(i)
