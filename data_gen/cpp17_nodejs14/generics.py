import random

from data_gen.mask_tokens import AI_GENERIC, AI_USER_TYPE
from data_gen.cpp17_nodejs14.cpp import CPP_PRIM_TYPES
from data_gen.utils import join


def gen_type_generics():
    """Generate type generics (e.g. `<T, K>`)."""

    generics = []
    generics_range = range(0, 4)
    generics_count = random.choices(generics_range, weights=(75, 15, 10, 5), k=1)[0]

    for i in range(generics_count):
        generics.append(AI_GENERIC)

    if generics_count > 0:
        return '<' + join(generics, ', ') + '>'

    return ''


def gen_provided_generics():
    """Generate provided generics (e.g. `<int, char>`)."""

    generics = []
    generics_range = range(0, 4)
    generics_count = random.choices(generics_range, weights=(75, 15, 10, 5), k=1)[0]
    generic_types = CPP_PRIM_TYPES.copy()
    generic_types.append(AI_USER_TYPE)

    for i in range(generics_count):
        generics.append(random.choice(generic_types))

    if generics_count > 0:
        return '<' + join(generics, ', ') + '>'

    return ''
