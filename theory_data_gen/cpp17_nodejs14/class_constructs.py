from tqdm import tqdm
from theory_data_gen.common import gen_val_list, gen_mask_token


def gen_class_construct_pair():
    """Generate a class construction pair."""

    args, _ = gen_val_list(mask_index=1)
    construct = f'new {gen_mask_token(0)}({args});'
    return construct


def gen_class_constructs(count):
    """Generate all class construction data."""

    data = []

    for _ in tqdm(range(count), desc='Generating class construction statements'):
        construct = gen_class_construct_pair()
        item = {'source': construct, 'target': construct}
        data.append(item)

    return data
