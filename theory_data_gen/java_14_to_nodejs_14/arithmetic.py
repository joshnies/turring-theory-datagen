import random

from tqdm import tqdm

from theory_data_gen.common import gen_item, add_mask_indices
from theory_data_gen.constants import MASK_TOKEN, ARITHMETIC_OPS
from theory_data_gen.utils import join_rand
from .java import JAVA_BOOL_OPS


def gen_arithmetic(should_add_mask_indices=False, only_bool=False):
    """Generate an arithmetic expression. Can contain boolean operators."""

    # Get operators
    ops = []

    if not only_bool:
        ops.extend(ARITHMETIC_OPS)

    ops.extend(JAVA_BOOL_OPS)

    # Add spacing to operators
    ops = list(map(lambda o: f' {o} ', ops))

    # Generate value list
    val_range = range(0, 11)
    val_count = random.choices(val_range, weights=(80, 70, 60, 40, 30, 20, 5, 4, 3, 2, 1), k=1)[0]
    vals = []

    for i in range(val_count):
        vals.append(MASK_TOKEN)

    source = join_rand(vals, ops)

    # Convert ops for target
    target = source.replace('==', '===')
    target = target.replace('!=', '!==')

    # Add mask indices
    if should_add_mask_indices:
        source, _ = add_mask_indices(source)
        target, _ = add_mask_indices(target)

    return source, target


def gen_rogue_arithmetic(count: int):
    """Generate rogue arithmetic expressions."""

    data = []

    for _ in tqdm(range(count), desc='Generating arithmetic/boolean expressions'):
        s, t = gen_arithmetic(should_add_mask_indices=True)
        data.append(gen_item(s, t))

    return data
