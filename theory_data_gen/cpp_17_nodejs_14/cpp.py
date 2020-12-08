import random

from theory_data_gen.mask_tokens import AI_USER_TYPE
from theory_data_gen.utils import join

# C++ primitive types (not replaced with mask token)
CPP_PRIM_TYPES = [
    'bool',
    'char',
    'int',
    'float',
    'double',
    'void',
    'wchar_t',
    'unsigned char',
    'signed char',
    'unsigned int',
    'signed int',
    'short int',
    'unsigned short int',
    'signed short int',
    'long int',
    'signed long int',
    'unsigned long int',
    'long long int',
    'unsigned long long int',
    'long double',
    'std::string',
    'std::array',
    'std::vector'
]

# C++ generic types
CPP_GENERIC_TYPES = [
    'std::array',
    'std::vector'
]


def gen_cpp_generic_type():
    """Generates a random C++ type with generic arguments."""

    generic_types = CPP_GENERIC_TYPES.copy()
    generic_types.append(AI_USER_TYPE)

    prim_types = CPP_PRIM_TYPES.copy()
    prim_types.append(AI_USER_TYPE)

    base_type = random.choice(generic_types)
    args = []

    # Generate generic args
    for i in range(1, 5):
        args.append(random.choice(prim_types))

    args = join(args, ', ')

    return f'{base_type}<{args}>'
