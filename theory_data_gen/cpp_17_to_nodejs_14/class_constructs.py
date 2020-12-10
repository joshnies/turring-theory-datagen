from tqdm import tqdm

from theory_data_gen.common import add_mask_indices
from theory_data_gen.constants import MASK_TOKEN
from .entity_chains import gen_entity_chain_pair
from .val_lists import gen_val_list


def __gen_class_construct_pair():
    """Generate a class construction pair."""

    args = gen_val_list(entity_chain_callback=gen_entity_chain_pair)
    construct = f'new {MASK_TOKEN}({args});'
    construct, _ = add_mask_indices(construct)

    return construct


def gen_class_constructs(count):
    """Generate class construction statements."""

    data = []

    for _ in tqdm(range(count), desc='Generating class construction statements'):
        construct = __gen_class_construct_pair()
        item = {'source': construct, 'target': construct}
        data.append(item)

    return data
