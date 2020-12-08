import random
import re

from theory_data_gen.mask_tokens import AI_VAL, MASK_TOKEN
from theory_data_gen.utils import join


def gen_mask_token(i):
    """Generate mask token with index."""

    return MASK_TOKEN.replace('n', str(i), 1)


def gen_val_list():
    """Generate a value list."""

    # TODO: Add support for arithmetic values
    # TODO: Add support for entity chain values

    val_range = range(0, 11)
    val_count = random.choices(val_range, weights=(80, 70, 60, 40, 30, 20, 5, 4, 3, 2, 1), k=1)[0]
    vals = []

    # Generate value list
    for i in range(val_count):
        vals.append(MASK_TOKEN)

    return join(vals, ', ')


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
