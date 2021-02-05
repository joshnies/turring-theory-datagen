from tqdm import tqdm

from .conditionals import gen_condition
from ...common import gen_mask_token, gen_item, add_mask_indices


def __gen_loop_items():
    """Generate loop structure items."""

    # Generate conditions
    src_condition, tar_condition = gen_condition()

    # Generate item
    src = f'PERFORM {gen_mask_token(0)} UNTIL {src_condition}'
    src, _ = add_mask_indices(src, start_index=1)

    tar = f'while(!{tar_condition}) {gen_mask_token(0)}();'
    tar, _ = add_mask_indices(tar, start_index=1)

    return [gen_item(src, tar)]


def gen_loops(write, count: int):
    """
    Generate loop structures.

    :param write: CSV output write function.
    :param count: Number of conditional statements to generate.
    """

    items = [
        (
            f'PERFORM {gen_mask_token(0)} UNTIL {gen_mask_token(1)}',
            f'while(!{gen_mask_token(1)}) {gen_mask_token(0)}();'
        )
    ]

    items = list(map(lambda item: gen_item(item[0], item[1]), items))

    # Generate items
    for _ in tqdm(range(count), desc='Generating loops'):
        items.extend(__gen_loop_items())

    # Write items to output
    for i in items:
        write(i)