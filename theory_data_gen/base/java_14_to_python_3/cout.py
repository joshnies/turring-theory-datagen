import random

from theory_data_gen.common import gen_item, gen_mask_token
from .arithmetic import gen_arithmetic
from .class_constructs import gen_class_construct_pair
from .entity_chains import gen_entity_chain_pair


def __gen_cout_item(val: str = None):
    """Generate console output item."""

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

    return gen_item(
        f'System.out.println({src_val});',
        f'print({tar_val})'
    )


def gen_couts(write, count: int):
    """Generate console output statements."""

    # Generate with specified value
    write(__gen_cout_item(gen_mask_token(0)))

    # Generate with random value
    for _ in range(count):
        write(__gen_cout_item())
