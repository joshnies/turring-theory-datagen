from tqdm import tqdm

from theory_data_gen.common import add_mask_indices, gen_mask_token, gen_item
from theory_data_gen.constants import MASK_TOKEN


def __gen_to_addition(count: int):
    """Generate "TO" syntax addition items."""

    added_tokens = [MASK_TOKEN] * count

    # Generate source
    src_added_tokens = ' '.join(added_tokens)
    src = f'ADD {src_added_tokens} TO {MASK_TOKEN}.'
    src, src_last_idx = add_mask_indices(src)

    # Generate target
    tar_added_tokens = ' + '.join(added_tokens)
    tar = f'{gen_mask_token(src_last_idx)}.Add({tar_added_tokens});'
    tar, _ = add_mask_indices(tar)

    # Generate item
    return gen_item(src, tar)


def __gen_giving_addition(count: int):
    """Generate "GIVING" syntax addition items."""

    added_tokens = [MASK_TOKEN] * count

    # Generate source
    src_added_tokens = ' '.join(added_tokens)
    src = f'ADD {src_added_tokens} GIVING {MASK_TOKEN}.'
    src, src_last_idx = add_mask_indices(src)

    # Generate target
    tar_added_tokens = ' + '.join(added_tokens)
    tar = f'{gen_mask_token(src_last_idx)}.Set({tar_added_tokens});'
    tar, _ = add_mask_indices(tar)

    # Generate item
    return gen_item(src, tar)


def gen_arithmetic(write):
    """
    Generate arithmetic expressions.

    :param write: CSV output write function.
    """

    items = list()

    # Generate items
    for count in tqdm(range(1, 11), desc='Generating arithmetic'):
        items.append(__gen_to_addition(count))
        items.append(__gen_giving_addition(count))

        # TODO: Implement subtraction
        # TODO: Implement multiplication
        # TODO: Implement division

    # Write items
    for i in items:
        write(i)
