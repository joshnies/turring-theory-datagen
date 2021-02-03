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


def __gen_from_subtraction():
    """Generate "FROM" syntax subtraction items."""

    # Generate source
    src = f'SUBTRACT {gen_mask_token(0)} FROM {gen_mask_token(1)}.'

    # Generate target
    tar = f'{gen_mask_token(1)}.Subtract({gen_mask_token(0)});'

    # Generate item
    return gen_item(src, tar)


def __gen_giving_subtraction():
    """Generate "GIVING" syntax subtraction items."""

    # Generate source
    src = f'SUBTRACT {gen_mask_token(0)} FROM {gen_mask_token(1)} GIVING {gen_mask_token(2)}.'

    # Generate target
    tar = f'{gen_mask_token(2)}.Set({gen_mask_token(1)} - {gen_mask_token(0)});'

    # Generate item
    return gen_item(src, tar)


def __gen_by_mult():
    """Generate "BY" syntax multiplication items."""

    # Generate source
    src = f'MULTIPLY {gen_mask_token(0)} BY {gen_mask_token(1)}.'

    # Generate target
    tar = f'{gen_mask_token(0)}.MultiplyBy({gen_mask_token(1)});'

    # Generate item
    return gen_item(src, tar)


def __gen_giving_mult():
    """Generate "GIVING" syntax multiplication items."""

    # Generate source
    src = f'MULTIPLY {gen_mask_token(0)} BY {gen_mask_token(1)} GIVING {gen_mask_token(2)}.'

    # Generate target
    tar = f'{gen_mask_token(2)}.Set({gen_mask_token(0)} * {gen_mask_token(1)});'

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
        items.append(__gen_from_subtraction())
        items.append(__gen_giving_subtraction())
        items.append(__gen_by_mult())
        items.append(__gen_giving_mult())

        # TODO: Implement division
        # TODO: Implement COMPUTE source keyword
        # TODO: Implement ROUNDED source keyword

    # Write items
    for i in items:
        write(i)
