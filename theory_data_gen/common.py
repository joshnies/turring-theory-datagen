import random

from theory_data_gen.mask_tokens import AI_VAL
from theory_data_gen.utils import join


def gen_val_list():
    """Generate a value list."""

    val_range = range(0, 11)
    val_count = random.choices(val_range, weights=(80, 70, 60, 40, 30, 20, 5, 4, 3, 2, 1), k=1)[0]
    vals = []

    # Generate value list
    for i in range(val_count):
        vals.append(AI_VAL)

    return join(vals, ', ')


def gen_item(source, target=None):
    """Generate dataset item."""

    if target is None:
        target = source

    return {'source': source, 'target': target}


def deduplicate(data):
    """Deduplicate data."""

    return [dict(t) for t in {tuple(d.items()) for d in data}]
