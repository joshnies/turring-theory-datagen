import random

from tqdm import tqdm
from data_gen.mask_tokens import AI_CLASS_NAME, AI_EXTRACTION, AI_INHERITED_CLASS_NAME
from data_gen.cpp17_nodejs14.generics import gen_type_generics
from data_gen.utils import join


def gen_class_inheritance():
    """Generate a C++ class inheritance sequence."""

    access_modifier = random.choice(['public', 'private', 'protected'])
    return f'{access_modifier} {AI_INHERITED_CLASS_NAME}'


def gen_class_pair():
    """Generate a class pair."""

    abstract = 'abstract ' if bool(random.getrandbits(1)) else ''

    # Generate generics
    generics = gen_type_generics()

    # Generate inheritance
    inheritance_range = range(0, 11)
    inheritance_count = random.choices(inheritance_range, weights=(80, 70, 60, 40, 30, 20, 5, 4, 3, 2, 1), k=1)[0]
    inheritance_prefix = ' : ' if inheritance_count > 0 else ''
    inheritance = []

    for i in range(inheritance_count):
        inheritance.append(gen_class_inheritance())

    inheritance = join(inheritance, ', ')

    body = AI_EXTRACTION if bool(random.getrandbits(1)) else ''

    source = f'{abstract}class {AI_CLASS_NAME}{generics}{inheritance_prefix}{inheritance} {{{body}}}'
    target = f'class {AI_CLASS_NAME} {{{body}}}'
    return source, target


def gen_classes(count):
    """Generate all class data."""

    data = []

    for _ in tqdm(range(count), desc='Generating classes'):
        (source, target) = gen_class_pair()
        item = {'source': source, 'target': target}
        data.append(item)

    return data
