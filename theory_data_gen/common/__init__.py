import random
import re

from theory_data_gen.constants import MASK_TOKEN


def gen_mask_token(i):
    """Generate mask token with index."""

    return MASK_TOKEN.replace('n', str(i), 1)


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


def gen_item(source, target=None):
    """Generate dataset item."""

    if target is None:
        target = source

    return {'source': source, 'target': target}


def add_open_bracket(item, src_only=False):
    """Add open bracket (`{`) to an item."""

    add = ' {'
    src = item['source'] + add
    tar = item['target'] + add if not src_only else item['target']

    return gen_item(src, tar)


def gen_type_generics():
    """Generate type generics (e.g. `<T, K>`)."""

    generics = []
    generics_range = range(0, 4)
    generics_count = random.choices(generics_range, weights=(75, 15, 10, 5), k=1)[0]

    for i in range(generics_count):
        generics.append(MASK_TOKEN)

    if generics_count > 0:
        return '<' + ', '.join(generics) + '>'

    return ''
