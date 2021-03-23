import random

from . import gen_type_generics
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
    'String',
    'Object'
]

JAVA_PRIM_TYPES_W_MASK = JAVA_PRIM_TYPES.copy()
JAVA_PRIM_TYPES_W_MASK.append(MASK_TOKEN)

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

JAVA_GENERIC_TYPES_W_MASK = JAVA_GENERIC_TYPES.copy()
JAVA_GENERIC_TYPES_W_MASK.append(MASK_TOKEN)

# Java access modifiers
JAVA_ACCESS_MODIFIERS = [
    'private',
    'protected',
    'public'
]


def gen_provided_generics():
    """
    Generate provided generics (e.g. `<int, char>`).

    :returns: Provided generics sequence.
    """

    generics = list()
    generics_range = range(0, 4)
    generics_count = random.choices(generics_range, weights=(75, 15, 10, 5), k=1)[0]

    for i in range(generics_count):
        generics.append(random.choice(JAVA_PRIM_TYPES_W_MASK))

    # NOTE: Empty generics are allowed in Java (e.g. `<>`)
    joined_generics = ', '.join(generics)
    return f'<{joined_generics}>'


def gen_java_generic_type(use_provided_types: bool = True):
    """
    Generates a random Java type with generic arguments.

    :param use_provided_types: Whether to use provided types (e.g. `<int, string>` instead of `<T, K>`).
    :returns: Generic type.
    """

    base_type = random.choice(JAVA_GENERIC_TYPES_W_MASK)
    generic = gen_provided_generics() if use_provided_types else gen_type_generics()

    return f'{base_type}{generic}'


def gen_inheritance(count):
    """
    Generate a Java inheritance sequence.

    :param count: Number of extended classes.
    :returns: Inheritance sequence.
    """

    mask_tokens = list()

    for _ in range(count):
        new_token = gen_java_generic_type(use_provided_types=False) if bool(random.getrandbits(1)) else random.choice(
            JAVA_PRIM_TYPES_W_MASK)

        mask_tokens.append(new_token)

    extended_classes = ', '.join(mask_tokens)

    return f' extends {extended_classes}'


def gen_interface_implementations(count):
    """Generate a Java interface implementation sequence.

    :param count: Number of implemented interfaces.
    :returns: Interface implementation sequence.
    """

    mask_tokens = list()

    for _ in range(count):
        new_token = gen_java_generic_type(use_provided_types=False) if bool(random.getrandbits(1)) else random.choice(
            JAVA_PRIM_TYPES_W_MASK)

        mask_tokens.append(new_token)

    implemented_interfaces = ', '.join(mask_tokens)

    return f' implements {implemented_interfaces}'
