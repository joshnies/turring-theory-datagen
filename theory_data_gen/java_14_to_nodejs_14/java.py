import random

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
    'char',
    'String'
]

# Java generic types
JAVA_GENERIC_TYPES = [
    'Array',
    'List',
    'Map',
    'HashMap'
]

# Java boolean operators
JAVA_BOOL_OPS = ['==', '!=', '>', '>=', '<', '<=', '&&', '||']


def gen_java_generic_type():
    """Generates a random Java type with generic arguments."""

    base_type = random.choice(JAVA_GENERIC_TYPES)
    args = []

    # Generate generic args
    for i in range(1, 5):
        args.append(random.choice(JAVA_PRIM_TYPES))

    args = join(args, ', ')

    return f'{base_type}<{args}>'
