import random

from theory_data_gen.utils import join

# C++ primitive types (not replaced with mask token)
CPP_PRIM_TYPES = [
    'auto',
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

# C++ chain notation operators
CPP_CHAIN_OPS = ['.', '->', '::']

# C++ boolean operators
CPP_BOOL_OPS = ['==', '!=', '>', '>=', '<', '<=', '&&', '||']


def gen_cpp_generic_type():
    """Generates a random C++ type with generic arguments."""

    base_type = random.choice(CPP_GENERIC_TYPES)
    args = []

    # Generate generic args
    for i in range(1, 5):
        args.append(random.choice(CPP_PRIM_TYPES))

    args = join(args, ', ')

    return f'{base_type}<{args}>'
