import random

from constants import AI_CLASS_NAME, AI_EXTRACTION, AI_INHERITED_CLASS_NAME, AI_GENERIC
from utils import join


def gen_class_inheritance():
    """Generate a C++ class inheritance sequence."""

    access_modifier = random.choice(['public', 'private', 'protected'])
    return f'{access_modifier} {AI_INHERITED_CLASS_NAME}'


def gen_class_pair():
    """Generate a class pair."""

    abstract = 'abstract ' if bool(random.getrandbits(1)) else ''

    # Generate generics
    generics_range = range(0, 4)
    generics_count = random.choices(generics_range, weights=(75, 15, 10, 5), k=1)[0]
    generics = []

    for i in range(generics_count):
        generics.append(AI_GENERIC)

    generics = '<' + join(generics, ', ') + '>'

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
    target = f'class {AI_CLASS_NAME}{generics} {{{body}}}'
    return source, target


def gen_classes(count):
    """Generate all class data."""

    data = []

    for i in range(count):
        (source, target) = gen_class_pair()
        item = {'source': source, 'target': target}

        if item not in data:
            data.append(item)

    return data
