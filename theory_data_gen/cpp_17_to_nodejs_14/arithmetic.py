import random

from theory_data_gen.constants import MASK_TOKEN, ARITHMETIC_OPS
from theory_data_gen.utils import join_rand


def gen_arithmetic():
    """Generate an arithmetic expression."""

    val_range = range(0, 11)
    val_count = random.choices(val_range, weights=(80, 70, 60, 40, 30, 20, 5, 4, 3, 2, 1), k=1)[0]
    vals = []

    # Generate value list
    for i in range(val_count):
        vals.append(MASK_TOKEN)

    seq = join_rand(vals, ARITHMETIC_OPS)

    return seq
