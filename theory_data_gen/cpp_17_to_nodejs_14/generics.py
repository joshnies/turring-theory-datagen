import random

from theory_data_gen.constants import MASK_TOKEN
from theory_data_gen.utils import join
from .cpp import CPP_PRIM_TYPES


def gen_type_generics():
    """Generate type generics (e.g. `<T, K>`)."""

    generics = []
    generics_range = range(0, 4)
    generics_count = random.choices(generics_range, weights=(75, 15, 10, 5), k=1)[0]

    for i in range(generics_count):
        generics.append(MASK_TOKEN)

    if generics_count > 0:
        return '<' + join(generics, ', ') + '>'

    return ''


def gen_provided_generics():
    """Generate provided generics (e.g. `<int, char>`)."""

    generics = []
    generics_range = range(0, 4)
    generics_count = random.choices(generics_range, weights=(75, 15, 10, 5), k=1)[0]
    generic_types = CPP_PRIM_TYPES.copy()
    generic_types.append(MASK_TOKEN)

    for i in range(generics_count):
        generics.append(random.choice(generic_types))

    if generics_count > 0:
        return '<' + join(generics, ', ') + '>'

    return ''
