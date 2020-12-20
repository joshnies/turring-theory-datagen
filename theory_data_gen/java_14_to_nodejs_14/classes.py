import random

from tqdm import tqdm

from theory_data_gen.common import gen_mask_token, add_mask_indices, add_open_bracket, gen_item
from theory_data_gen.constants import MASK_TOKEN
from theory_data_gen.utils import join
from .generics import gen_type_generics


def gen_class_inheritance():
    """Generate a Java class inheritance sequence."""

    access_modifier = random.choice(['public', 'private', 'protected'])
    return f'{access_modifier} {MASK_TOKEN}'


def gen_class_pairs():
    """Generate class pairs."""

    # Generate mask tokens
    m_class_name = gen_mask_token(0)

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

    source = f'{abstract}class {m_class_name}{generics}{inheritance_prefix}{inheritance}'
    source, _ = add_mask_indices(source, start_index=1)

    target = f'class {m_class_name}'

    item_wo_open_bracket = gen_item(source, target)
    item_w_open_bracket = add_open_bracket(item_wo_open_bracket)

    return [
        item_wo_open_bracket,
        item_w_open_bracket
    ]


def gen_classes(count):
    """Generate classes."""

    data = []

    for _ in tqdm(range(count), desc='Generating classes'):
        items = gen_class_pairs()
        data.extend(items)

    return data