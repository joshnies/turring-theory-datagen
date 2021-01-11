import random

from theory_data_gen.constants import MASK_TOKEN

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

# Java access modifiers
JAVA_ACCESS_MODIFIERS = [
    'private',
    'protected',
    'public'
]


def gen_provided_generics():
    """Generate provided generics (e.g. `<int, char>`)."""

    generics = list()
    generics_range = range(0, 4)
    generics_count = random.choices(generics_range, weights=(75, 15, 10, 5), k=1)[0]
    generic_types = JAVA_PRIM_TYPES.copy()
    generic_types.append(MASK_TOKEN)

    for i in range(generics_count):
        generics.append(random.choice(generic_types))

    if generics_count > 0:
        joined_generics = ', '.join(generics)
        return f'<{joined_generics}>'

    return ''


def gen_java_generic_type():
    """Generates a random Java type with generic arguments."""

    types = JAVA_GENERIC_TYPES.copy()
    types.append(MASK_TOKEN)

    arg_types = JAVA_PRIM_TYPES.copy()
    arg_types.append(MASK_TOKEN)

    base_type = random.choice(types)
    args = list()

    # Generate generic args
    for i in range(1, 5):
        args.append(random.choice(arg_types))

    args = ', '.join(args)

    return f'{base_type}<{args}>'


def gen_inheritance(count):
    """Generate a Java class inheritance sequence."""

    mask_tokens = list()

    for _ in range(count):
        new_token = gen_java_generic_type() if bool(random.getrandbits(1)) else MASK_TOKEN
        mask_tokens.append(new_token)

    extended_classes = ', '.join(mask_tokens)

    return f' extends {extended_classes}'


def gen_interface_implementations(count):
    """Generate a Java interface implementation sequence."""

    mask_tokens = list()

    for _ in range(count):
        new_token = gen_java_generic_type() if bool(random.getrandbits(1)) else MASK_TOKEN
        mask_tokens.append(new_token)

    implemented_interfaces = ', '.join(mask_tokens)

    return f' implements {implemented_interfaces}'
