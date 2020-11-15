import random

from constants import AI_GENERIC, AI_USER_TYPE
from cpp import CPP_PRIM_TYPES


def join(iterator, separator):
    """Join items in an iterator with a given separator."""

    it = map(str, iterator)
    separator = str(separator)
    string = next(it, '')
    for s in it:
        string += separator + s
    return string


def join_rand(iterator, separators):
    """Join items in an iterator with a random separator."""

    it = map(str, iterator)
    string = next(it, '')
    for s in it:
        string += random.choice(separators) + s
    return string


def gen_type_generics():
    """Generate type generics (e.g. `<T, K>`)."""

    # Generate generics
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

    # Generate generics
    generics = []

    generics_range = range(0, 4)
    generics_count = random.choices(generics_range, weights=(75, 15, 10, 5), k=1)[0]
    generic_types = CPP_PRIM_TYPES
    generic_types.append(AI_USER_TYPE)

    for i in range(generics_count):
        generics.append(random.choice(generic_types))

    if generics_count > 0:
        return '<' + join(generics, ', ') + '>'

    return ''
