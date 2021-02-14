from tqdm import tqdm

from .conditionals import gen_condition
from ...common import gen_mask_token, gen_item, add_mask_indices
from ...common.cobol import COBOL_BOOL_OP_MAP


def __gen_loop_items():
    """Generate loop structure items."""

    items = list()

    # Generate conditions
    src_condition, tar_condition = gen_condition()

    # Generate "UNTIL" item
    src = f'PERFORM {gen_mask_token(0)} UNTIL {src_condition}.'
    src, _ = add_mask_indices(src, start_index=1)

    tar = f'while(!{tar_condition}) {gen_mask_token(0)}();'
    tar, _ = add_mask_indices(tar, start_index=1)

    items.append(gen_item(src, tar))

    # Generate "WITH TEST BEFORE UNTIL" item
    src = f'PERFORM {gen_mask_token(0)} WITH TEST BEFORE UNTIL {src_condition}.'
    src, _ = add_mask_indices(src, start_index=1)

    tar = f'while(!{tar_condition}) {gen_mask_token(0)}();'
    tar, _ = add_mask_indices(tar, start_index=1)

    items.append(gen_item(src, tar))

    # Generate "WITH TEST AFTER UNTIL" item
    src = f'PERFORM {gen_mask_token(0)} WITH TEST AFTER UNTIL {src_condition}.'
    src, _ = add_mask_indices(src, start_index=1)

    tar = f'do {{ {gen_mask_token(0)}(); }} while(!{tar_condition});'
    tar, _ = add_mask_indices(tar, start_index=1)

    items.append(gen_item(src, tar))

    return items


def __gen_varying_loops():
    """Generate "VARYING UNTIL" loop structure items."""

    items = list()

    for src_op, tar_op in COBOL_BOOL_OP_MAP.items():
        src = f'PERFORM {gen_mask_token(0)} VARYING {gen_mask_token(1)} 1 BY 1 UNTIL {gen_mask_token(1)} {src_op} {gen_mask_token(2)}.',
        tar = f'while(!({gen_mask_token(1)} {tar_op} {gen_mask_token(2)})) {gen_mask_token(0)}();',
        items.append(gen_item(src, tar))

    return items


def gen_loops(write, count: int):
    """
    Generate loop structures.

    :param write: CSV output write function.
    :param count: Number of conditional statements to generate.
    """

    items = [
        (
            f'PERFORM {gen_mask_token(0)} UNTIL {gen_mask_token(1)}.',
            f'while(!{gen_mask_token(1)}) {gen_mask_token(0)}();',
        ),
    ]

    items = list(map(lambda item: gen_item(item[0], item[1]), items))

    # Generate "PERFORM VARYING" loop items
    items.extend(__gen_varying_loops())

    # Generate all other items
    for _ in tqdm(range(count), desc='Generating loops'):
        items.extend(__gen_loop_items())

    # Write items to output
    for i in items:
        write(i)