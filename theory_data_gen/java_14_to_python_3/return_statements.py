import random

from tqdm import tqdm

from theory_data_gen.common import gen_item, gen_mask_token
from theory_data_gen.constants import MASK_TOKEN
from .arithmetic import gen_arithmetic
from .entity_chains import gen_entity_chain_pair


def gen_return():
    """Generate a return statement."""

    # Generate default values
    selection = random.choice(range(3))

    if selection == 0:
        # Mask token
        val = MASK_TOKEN
        source_def_val = val
        target_def_val = val
    elif selection == 1:
        # Arithmetic/boolean expression
        source_def_val, target_def_val = gen_arithmetic()
    else:
        # Entity chain
        source_def_val, target_def_val = gen_entity_chain_pair(add_semicolon=False, should_add_mask_indices=False)

    source = f'return {source_def_val};'
    target = f'return {target_def_val}'

    return source, target


def gen_returns(write, count: int):
    """Generate return statements."""

    item = gen_item(f'return {gen_mask_token(0)};')
    write(item)

    # Generate expression-based "return" statements
    for _ in tqdm(range(count), desc='Generating "return" statements'):
        src, tar = gen_return()
        item = gen_item(src, tar)
        write(item)
