import random

from tqdm import tqdm

from theory_data_gen.common import gen_mask_token, add_mask_indices, add_scope_open_token, gen_item, gen_type_generics
from theory_data_gen.common.java import JAVA_ACCESS_MODIFIERS
from theory_data_gen.constants import MASK_TOKEN
from theory_data_gen.utils import join
from .lvp import gen_modifier_permutations


def gen_class_inheritance():
    """Generate a Java class inheritance sequence."""

    access_modifier = random.choice(JAVA_ACCESS_MODIFIERS)
    return f'{access_modifier} {MASK_TOKEN}'


def __gen_class_items():
    """Generate class items."""

    # Generate mask tokens
    m_class_name = gen_mask_token(0)

    abstract = 'abstract ' if bool(random.getrandbits(1)) else ''

    # Generate generics
    generics = gen_type_generics()

    # Generate inheritance
    inheritance_range = range(0, 11)
    inheritance_count = random.choices(inheritance_range, weights=(80, 70, 60, 40, 30, 20, 5, 4, 3, 2, 1), k=1)[0]
    inheritance_prefix = ' : ' if inheritance_count > 0 else ''
    inheritance = list()

    for _ in range(inheritance_count):
        inheritance.append(gen_class_inheritance())

    inheritance = join(inheritance, ', ')

    source = f'{abstract}class {m_class_name}{generics}{inheritance_prefix}{inheritance}'
    source, _ = add_mask_indices(source, start_index=1)

    target = f'class {m_class_name}'

    items = gen_modifier_permutations(gen_item(source, target), include_static=False)
    items.extend(
        list(map(lambda i: add_scope_open_token(i), items))
    )

    return items


def gen_classes(write, count):
    """Generate classes."""

    for _ in tqdm(range(count), desc='Generating classes'):
        for i in __gen_class_items():
            write(i)
