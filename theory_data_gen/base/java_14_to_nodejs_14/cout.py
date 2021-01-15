import random

from theory_data_gen.common import gen_item, gen_mask_token
from .arithmetic import gen_arithmetic
from .class_constructs import gen_class_construct_pair
from .entity_chains import gen_entity_chain_pair


def __gen_cout_pair(val: str = None):
    """
    Generate console output pair.

    :param val: Value of the console output statement.
    :returns: Console output statement pair.
    """

    # Generate default values
    selection = random.choice(range(4)) if val is None else 0

    if selection == 0:
        # Supplied value
        src_val = val
        tar_val = val
    elif selection == 1:
        # Arithmetic/boolean expression
        src_val, tar_val = gen_arithmetic()
    elif selection == 2:
        # Entity chain
        src_val, tar_val = gen_entity_chain_pair(add_semicolon=False, should_add_mask_indices=False)
    else:
        # Class construct
        src_val, tar_val = gen_class_construct_pair(add_semicolon=False, should_add_mask_indices=False)

    src = f'System.out.println({src_val});'
    tar = f'console.log({tar_val});'
    return src, tar


def gen_couts(write, count: int):
    """Generate console output statements."""

    # Generate with specified value
    src, tar = __gen_cout_pair(gen_mask_token(0))
    item = gen_item(src, tar)
    write(item)

    # Generate with random value
    for _ in range(count):
        src, tar = __gen_cout_pair()
        item = gen_item(src, tar)
        write(item)
