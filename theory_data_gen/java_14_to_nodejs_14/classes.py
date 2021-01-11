import random

from tqdm import tqdm

from theory_data_gen.common import add_mask_indices, add_scope_open_token, gen_item, gen_mask_token
from theory_data_gen.constants import MASK_TOKEN
from .common import gen_interface_implementations, gen_inheritance
from .generics import gen_type_generics
from .java import gen_modifier_permutations


def gen_class_pairs():
    """Generate class pairs."""

    abstract = 'abstract ' if bool(random.getrandbits(1)) else ''

    # Generate generics
    generics = gen_type_generics()

    # Generate inheritance for interface
    # interface_inheritance_range = range(0, 11)
    # interface_inheritance_count = \
    # random.choices(interface_inheritance_range, weights=(80, 70, 60, 40, 30, 20, 5, 4, 3, 2, 1), k=1)[0]
    # interface_inheritance = gen_class_inheritance(
    #     interface_inheritance_count) if interface_inheritance_count > 0 else ''

    # Generate interface implementations
    interface_impl_range = range(0, 11)
    interface_impl_count = random.choices(interface_impl_range, weights=(80, 70, 60, 40, 30, 20, 5, 4, 3, 2, 1), k=1)[0]
    interface_impl = gen_interface_implementations(interface_impl_count) if interface_impl_count > 0 else ''

    # Generate base item
    src_base = f'{abstract}class {MASK_TOKEN}{generics}{interface_impl}'
    tar_base = f'class {MASK_TOKEN}'

    src_base, src_base_last_idx = add_mask_indices(src_base)
    tar_base, _ = add_mask_indices(tar_base)
    item_base = gen_item(src_base, tar_base)

    items = gen_modifier_permutations(item_base, include_static=False)
    items.extend(
        list(map(lambda i: add_scope_open_token(i), items))
    )

    # Generate item with inheritance
    inheritance = gen_inheritance(1)
    src_inher = f'{abstract}class {MASK_TOKEN}{generics}{interface_impl}{inheritance}'
    tar_inher = f'class {gen_mask_token(0)}{inheritance}'

    src_inher, _ = add_mask_indices(src_inher)
    tar_inher, _ = add_mask_indices(tar_inher, start_index=src_base_last_idx + 1)
    item_inher = gen_item(src_inher, tar_inher)

    all_inher_items = gen_modifier_permutations(item_inher, include_static=False)
    items.extend(all_inher_items)
    items.extend(
        list(map(lambda i: add_scope_open_token(i), all_inher_items))
    )

    return items


def gen_classes(write, count):
    """Generate classes."""

    for _ in tqdm(range(count), desc='Generating classes'):
        for i in gen_class_pairs():
            write(i)
