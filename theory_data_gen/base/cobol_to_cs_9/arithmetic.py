from tqdm import tqdm

from ...common import add_mask_indices, gen_mask_token, gen_item
from ...constants import MASK_TOKEN
from ...utils import join_rand


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


def __gen_into_division():
    """Generate "INTO" syntax division items."""

    items = list()

    # Generate normal
    src = f'DIVIDE {gen_mask_token(0)} INTO {gen_mask_token(1)}.'
    tar = f'{gen_mask_token(1)}.DivideBy({gen_mask_token(0)});'
    items.append(gen_item(src, tar))

    # Generate with remainder
    src = src[:-1] + f' REMAINDER {gen_mask_token(2)}.'
    tar = f'{gen_mask_token(1)}.DivideByWithRemainder({gen_mask_token(0)}, {gen_mask_token(2)});'
    items.append(gen_item(src, tar))

    return items


def __gen_by_division():
    """Generate "BY" syntax division items."""

    items = list()

    # Generate normal
    src = f'DIVIDE {gen_mask_token(0)} BY {gen_mask_token(1)}.'
    tar = f'{gen_mask_token(0)}.DivideBy({gen_mask_token(1)});'
    items.append(gen_item(src, tar))

    # Generate with remainder
    src = src[:-1] + f' REMAINDER {gen_mask_token(2)}.'
    tar = f'{gen_mask_token(0)}.DivideByWithRemainder({gen_mask_token(1)}, {gen_mask_token(2)});'
    items.append(gen_item(src, tar))

    return items


def __gen_compute(count: int, permutation_count: int):
    """Generate "COMPUTE" syntax arithmetic items."""

    items = list()
    mask_start_idx = 1

    # Generate math sequences
    math_ops = ['+', '-', '*', '/', '**']
    math_ops = list(map(lambda o: f' {o} ', math_ops))  # add spaces as padding for each operator
    tokens = [MASK_TOKEN] * count

    # TODO: Add support for parenthese-grouped arithmetic

    for _ in range(permutation_count):
        math_seq = join_rand(tokens, math_ops)

        # Generate source
        src = f'COMPUTE {gen_mask_token(0)} = {math_seq}.'
        src, _ = add_mask_indices(src, start_index=mask_start_idx)

        # Generate target
        tar = f'{gen_mask_token(0)}.Set({math_seq});'
        tar, _ = add_mask_indices(tar, start_index=mask_start_idx)

        # Generate item
        items.append(gen_item(src, tar))

    return items


def gen_arithmetic(write, compute_permut_count: int):
    """
    Generate arithmetic expressions.

    :param write: CSV output write function.
    :param compute_count: Number of COBOL "COMPUTE" arithmetic expressions to generate.
    """

    items = list()

    # Generate items
    items.append(__gen_from_subtraction())
    items.append(__gen_giving_subtraction())
    items.append(__gen_by_mult())
    items.append(__gen_giving_mult())
    items.extend(__gen_into_division())
    items.extend(__gen_by_division())

    for count in tqdm(range(1, 11), desc='Generating standard arithmetic'):
        # Generate items with static counts
        items.append(__gen_to_addition(count))
        items.append(__gen_giving_addition(count))

        # Generate items with arbitrary counts (random generation)
        items.extend(__gen_compute(count, compute_permut_count))

        # TODO: Implement ROUNDED source keyword

    # Write items
    for i in items:
        write(i)
