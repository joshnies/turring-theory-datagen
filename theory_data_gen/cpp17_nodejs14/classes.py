import random
from tqdm import tqdm

from common import gen_mask_token
from theory_data_gen.cpp17_nodejs14.generics import gen_type_generics
from theory_data_gen.utils import join


def gen_class_inheritance(mask_index):
    """Generate a C++ class inheritance sequence."""

    access_modifier = random.choice(['public', 'private', 'protected'])
    return f'{access_modifier} {gen_mask_token(mask_index)}'


def gen_class_pair():
    """Generate a class pair."""

    # Generate mask tokens
    m_class_name = gen_mask_token(0)

    abstract = 'abstract ' if bool(random.getrandbits(1)) else ''

    # Generate generics
    generics, last_mask_idx = gen_type_generics(1)

    # Generate inheritance
    inheritance_range = range(0, 11)
    inheritance_count = random.choices(inheritance_range, weights=(80, 70, 60, 40, 30, 20, 5, 4, 3, 2, 1), k=1)[0]
    inheritance_prefix = ' : ' if inheritance_count > 0 else ''
    inheritance = []

    for i in range(inheritance_count):
        inheritance.append(gen_class_inheritance(i + last_mask_idx + 1))

    inheritance = join(inheritance, ', ')

    source = f'{abstract}class {m_class_name}{generics}{inheritance_prefix}{inheritance} {{'
    target = f'class {m_class_name} {{'
    return source, target


def gen_classes(count):
    """Generate all class data."""

    data = []

    for _ in tqdm(range(count), desc='Generating classes'):
        (source, target) = gen_class_pair()
        item = {'source': source, 'target': target}
        data.append(item)

    return data
