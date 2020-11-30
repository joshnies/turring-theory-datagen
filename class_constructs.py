from tqdm import tqdm
from common import gen_val_list
from constants import AI_CLASS_NAME


def gen_class_construct_pair():
    """Generate a class construction pair."""

    args = gen_val_list()
    construct = f'new {AI_CLASS_NAME}({args});'
    return construct


def gen_class_constructs(count):
    """Generate all class construction data."""

    data = []

    for _ in tqdm(range(count), desc='Generating class construction statements'):
        construct = gen_class_construct_pair()
        item = {'source': construct, 'target': construct}
        data.append(item)

    return data
