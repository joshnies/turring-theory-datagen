import random
import re

from theory_data_gen.constants import MASK_TOKEN


def gen_mask_token(i):
    """
    :returns: Mask token with the given index.
    """

    return MASK_TOKEN.replace('n', str(i), 1)


def add_mask_indices(seq: str, start_index=0):
    """
    Add indices to generic mask tokens in a sequence.

    :param seq: Sequence.
    :param start_index: First index for mask indices.

    :returns Tuple of sequence with indexed mask tokens, and last mask index.
    """

    result = seq
    count = len(re.findall(MASK_TOKEN, seq))

    for i in range(count):
        result = result.replace(MASK_TOKEN, gen_mask_token(start_index + i), 1)

    return result, start_index + count - 1


def gen_item(source, target=None):
    """
    Generate dataset item.

    :param source: Source sequence.
    :param target: Target sequence. If `None`, then `source` is used instead.

    :returns: Dictionary of 'source' and 'target' keys mapping to the relevant sequences.
    """

    if target is None:
        target = source

    return {'source': source, 'target': target}


def add_scope_open_token(item, src_token=' {', tar_token=' {'):
    """
    Add the opening scope token (e.g. `{` for most languages) to an item.

    :param item: Dataset item.
    :param src_token: Source opening scope token.
    :param tar_token: Target opening scope token.

    :returns: Dataset item with appended scope opening tokens.
    """

    src = item['source'] + src_token
    tar = item['target'] + tar_token

    return gen_item(src, tar)


def add_line_end_token(item, src_token=';', tar_token=';'):
    """
    Add the line end token (e.g. `;` for most languages) to an item.

    :param item: Dataset item.
    :param src_token: Source line end token.
    :param tar_token: Target line end token.

    :returns: Dataset item with appended line end tokens.
    """

    src = item['source'] + src_token
    tar = item['target'] + tar_token

    return gen_item(src, tar)


def gen_type_generics():
    """
    Generate type generics (e.g. `<T, K>`).

    :returns: Type generics sequence (including brackets), or an empty string if no generics were generated
    (random choice).
    """

    generics = list()
    generics_range = range(0, 4)
    generics_count = random.choices(generics_range, weights=(75, 15, 10, 5), k=1)[0]

    for i in range(generics_count):
        generics.append(MASK_TOKEN)

    if generics_count > 0:
        joined_generics = ', '.join(generics)
        return f'<{joined_generics}>'

    return ''
