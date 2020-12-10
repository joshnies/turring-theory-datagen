import random
from tqdm import tqdm

from theory_data_gen.common import gen_item, add_mask_indices
from theory_data_gen.constants import MASK_TOKEN, ARITHMETIC_OPS
from theory_data_gen.utils import join_rand


def __gen_arithmetic_exp():
    """Generate an arithmetic expression."""

    val_range = range(0, 11)
    val_count = random.choices(val_range, weights=(80, 70, 60, 40, 30, 20, 5, 4, 3, 2, 1), k=1)[0]
    vals = []

    # Generate value list
    for i in range(val_count):
        vals.append(MASK_TOKEN)

    seq = join_rand(vals, ARITHMETIC_OPS)
    seq, _ = add_mask_indices(seq)

    return seq


def gen_arithmetic(count: int):
    """Generate arithmetic expressions."""

    data = []

    for _ in tqdm(range(count), desc='Generating arithmetic expressions'):
        item = gen_item(__gen_arithmetic_exp())
        data.append(item)

    return data
