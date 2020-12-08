from tqdm import tqdm

from constants import MASK_TOKEN
from theory_data_gen.common import gen_val_list, gen_mask_token, add_mask_indices


def gen_class_construct_pair():
    """Generate a class construction pair."""

    args = gen_val_list()
    construct = f'new {MASK_TOKEN}({args});'
    construct, _ = add_mask_indices(construct)

    return construct


def gen_class_constructs(count):
    """Generate all class construction data."""

    data = []

    for _ in tqdm(range(count), desc='Generating class construction statements'):
        construct = gen_class_construct_pair()
        item = {'source': construct, 'target': construct}
        data.append(item)

    return data
