import re

from .constants import MASK_TOKEN


def gen_mask_token(i):
    """Generate mask token with index."""

    return MASK_TOKEN.replace('n', str(i), 1)


def gen_item(source, target=None):
    """Generate dataset item."""

    if target is None:
        target = source

    return {'source': source, 'target': target}


def deduplicate(data):
    """Deduplicate data."""

    return [dict(t) for t in {tuple(d.items()) for d in data}]


def add_mask_indices(seq: str, start_index=0):
    """
    Add indices to generic mask tokens in a sequence.

    :returns Tuple of sequence with indexed mask tokens, and last mask index.
    """

    result = seq
    count = len(re.findall(MASK_TOKEN, seq))

    for i in range(count):
        result = result.replace(MASK_TOKEN, gen_mask_token(start_index + i), 1)

    return result, start_index + count - 1
