import random

from theory_data_gen.common import gen_item, gen_mask_token
from .arithmetic import gen_arithmetic
from .class_constructs import gen_class_construct_pair
from .entity_chains import gen_entity_chain_pair


def __gen_cout_pair(val: str = None):
    """Generate console output pair."""

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

    source = f'System.out.println({src_val});'
    target = f'console.log({tar_val});'
    return source, target


def gen_couts(count: int):
    """Generate console output statements."""

    data = list()

    # Generate with specified value
    source, target = __gen_cout_pair(gen_mask_token(0))
    data.append(gen_item(source, target))

    # Generate with random value
    for _ in range(count):
        source, target = __gen_cout_pair()
        data.append(gen_item(source, target))

    return data
