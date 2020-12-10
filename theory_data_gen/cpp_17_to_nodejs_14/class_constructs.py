from tqdm import tqdm

from theory_data_gen.common import add_mask_indices, gen_item
from theory_data_gen.constants import MASK_TOKEN
from .entity_chains import gen_entity_chain_pair
from .val_lists import gen_val_list


def __gen_class_construct_pair():
    """Generate a class construction pair."""

    source_args, target_args = gen_val_list(entity_chain_callback=gen_entity_chain_pair)
    source = f'new {MASK_TOKEN}({source_args});'
    target = f'new {MASK_TOKEN}({target_args});'

    # Add mask indices
    source = add_mask_indices(source)
    target = add_mask_indices(target)

    return source, target


def gen_class_constructs(count):
    """Generate class construction statements."""

    data = []

    for _ in tqdm(range(count), desc='Generating class construction statements'):
        source, target = __gen_class_construct_pair()
        data.append(gen_item(source, target))

    return data
