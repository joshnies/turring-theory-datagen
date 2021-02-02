from itertools import permutations
from typing import List

from yaspin import yaspin

from theory_data_gen.common import gen_item, add_mask_indices
from theory_data_gen.constants import MASK_TOKEN


def __gen_write_line(val_list: List[str]):
    """Generate write line statement (includes newline)."""

    # Generate sources
    src_vals = ' '.join(val_list)
    src = f'DISPLAY {src_vals}.'
    src, _ = add_mask_indices(src)

    # Generate targets
    tar_vals = ' + '.join(val_list)
    tar = f'Console.WriteLine({tar_vals});'
    tar, _ = add_mask_indices(tar)

    # Generate item
    return gen_item(src, tar)


def __gen_write(val_list: List[str]):
    """Generate write statement (no newline)."""

    # Generate sources
    src_vals = ' '.join(val_list)
    src = f'DISPLAY {src_vals} WITH NO ADVANCING.'
    src, _ = add_mask_indices(src)

    # Generate targets
    tar_vals = ' + '.join(val_list)
    tar = f'Console.Write({tar_vals});'
    tar, _ = add_mask_indices(tar)

    # Generate item
    return gen_item(src, tar)


def gen_stdout(write):
    """
    Generate standard output statements.

    :param write: CSV output write function.
    """

    with yaspin(text='Generating stdout...', color='magenta'):
        for str_count in range(0, 5):
            for var_count in range(0, 5):
                # Skip case where both counts are 0
                if str_count == 0 and var_count == 0:
                    continue

                # Generate base value list
                vals = [f'"{MASK_TOKEN}"'] * str_count
                vals.extend([MASK_TOKEN] * var_count)

                # Generate permutations
                if str_count == 0 or var_count == 0:
                    val_lists = [vals]
                else:
                    val_lists = list(permutations(vals))
                    val_lists = list(map(lambda l: list(l), val_lists))

                for v in val_lists:
                    # Generate line write
                    item = __gen_write_line(v)
                    write(item)

                    # Generate same-line write
                    item = __gen_write(v)
                    write(item)
