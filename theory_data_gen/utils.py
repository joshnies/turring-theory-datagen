import random
from typing import List


def join(iterator: List[str], separator: str) -> str:
    """Join items in an iterator with a given separator."""

    it = map(str, iterator)
    separator = str(separator)
    string = next(it, '')

    for s in it:
        string += separator + s

    return string


def join_rand(iterator: List[str], separators: List[str]) -> str:
    """Join items in an iterator with a random separator."""

    it = map(str, iterator)
    string = next(it, '')

    for s in it:
        string += random.choice(separators) + s

    return string
