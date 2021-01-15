from tqdm import tqdm

from theory_data_gen.common import add_mask_indices, gen_item
from theory_data_gen.constants import MASK_TOKEN
from .entity_chains import gen_entity_chain_pair
from .val_lists import gen_val_list


def gen_class_construct_pair(add_semicolon=True, should_add_mask_indices=True):
    """Generate a class construction pair."""

    source_args, target_args = gen_val_list(entity_chain_callback=gen_entity_chain_pair)
    semicolon = ';' if add_semicolon else ''

    src = f'new {MASK_TOKEN}({source_args}){semicolon}'
    tar = f'{MASK_TOKEN}({target_args})'

    # Add mask indices
    if should_add_mask_indices:
        src, _ = add_mask_indices(src)
        tar, _ = add_mask_indices(tar)

    return src, tar


def gen_class_constructs(write, count):
    """Generate class construction statements."""

    for _ in tqdm(range(count), desc='Generating class construction statements'):
        src, tar = gen_class_construct_pair()
        item = gen_item(src, tar)
        write(item)
