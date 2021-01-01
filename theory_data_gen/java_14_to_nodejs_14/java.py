import random

from theory_data_gen.common import gen_item
from theory_data_gen.common.java import JAVA_ACCESS_MODIFIERS
from theory_data_gen.constants import MASK_TOKEN
from theory_data_gen.utils import join

# Java primitive types (not replaced with mask token)
JAVA_PRIM_TYPES = [
    'var',
    'void',
    'boolean',
    'byte',
    'short',
    'int',
    'float',
    'double',
    'long',
    'char',
    'Void',
    'Boolean',
    'Byte',
    'Short',
    'Integer',
    'Float',
    'Double',
    'Long',
    'Character',
    'String'
]

# Java generic types
JAVA_GENERIC_TYPES = [
    'Array',
    'ArrayList',
    'List',
    'LinkedList',
    'Set',
    'HashSet',
    'LinkedHashSet',
    'Map',
    'HashMap',
    'ConcurrentHashMap',
    'IdentityHashMap',
    'LinkedHashMap',
    'WeakHashMap'
]


def gen_java_generic_type():
    """Generates a random Java type with generic arguments."""

    types = JAVA_GENERIC_TYPES.copy()
    types.append(MASK_TOKEN)

    base_type = random.choice(types)
    args = []

    # Generate generic args
    for i in range(1, 5):
        arg_types = JAVA_PRIM_TYPES.copy()
        arg_types.append(MASK_TOKEN)
        args.append(random.choice(arg_types))

    args = join(args, ', ')

    return f'{base_type}<{args}>'


def gen_modifier_permutations(item, include_abstract=True, include_static=True):
    """
    Generate permutations of an item with prefixed modifiers (such as access modifiers).

    :param item: Item.
    :param include_abstract: Whether to generate permutations with "abstract" modifiers.
    :param include_static: Whether to generate permutations with "static" modifiers.

    :returns: List of item permutations with modifiers.
    """

    src = item['source']
    tar = item['target']
    new_items = []

    mods = JAVA_ACCESS_MODIFIERS.copy()
    mods.append('')

    for m in mods:
        new_items.append((f'{m} {src}', tar))

        if include_abstract:
            new_items.append((f'{m} abstract {src}', f'abstract {tar}'))

            if include_static:
                new_items.append((f'{m} static abstract {src}', f'static abstract {tar}'))
                new_items.append((f'{m} abstract static {src}', f'abstract static {tar}'))

        if include_static:
            new_items.append((f'{m} static {src}', f'static {tar}'))

    new_items = list(map(lambda i: gen_item(i[0].strip(), i[1].strip()), new_items))

    return new_items
