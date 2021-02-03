import random
from itertools import permutations
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
    """
    Join items in an iterator with a random separator.

    :param iterator: Iterator.
    :param separators: List of separators.
    :returns: Joined string.
    """

    it = map(str, iterator)
    string = next(it, '')

    for s in it:
        string += random.choice(separators) + s

    return string


def permutations_as_lists(iterable: List) -> List[List]:
    """
    Create permutations as lists instead of tuples.

    :param iterable: List.
    :returns: List of permutation lists.
    """

    result = list(permutations(iterable))
    return list(map(lambda r: list(r), result))
