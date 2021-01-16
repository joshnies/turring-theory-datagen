import random

from tqdm import tqdm

from theory_data_gen.common import gen_item, add_mask_indices
from theory_data_gen.constants import MASK_TOKEN, ARITHMETIC_OPS, BOOL_OPS
from theory_data_gen.utils import join_rand


def gen_arithmetic(should_add_mask_indices=False, only_bool=False):
    """Generate an arithmetic expression. Can contain boolean operators."""

    # Get operators
    ops = BOOL_OPS.copy()

    if not only_bool:
        ops.extend(ARITHMETIC_OPS)

    # Add spacing to operators
    ops = list(map(lambda o: f' {o} ', ops))

    # Generate value list
    val_range = range(1, 11)
    val_count = random.choices(val_range, weights=(80, 70, 60, 40, 30, 10, 4, 3, 2, 1), k=1)[0]
    vals = list()

    for _ in range(val_count):
        vals.append(MASK_TOKEN)

    src = join_rand(vals, ops)

    # Add mask indices
    if should_add_mask_indices:
        src, _ = add_mask_indices(src)

    return src, src


def gen_rogue_arithmetic(write, count: int):
    """Generate rogue arithmetic expressions."""

    for _ in tqdm(range(count), desc='Generating arithmetic/boolean expressions'):
        src, tar = gen_arithmetic(should_add_mask_indices=True)
        item = gen_item(src, tar)
        write(item)