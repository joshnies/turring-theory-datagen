import random


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
