import random

from theory_data_gen.common import gen_mask_token, gen_item, add_open_bracket
from .class_constructs import gen_class_construct_pair
from .entity_chains import gen_entity_chain_pair


def __gen_switch_structs(condition: str = None):
    """
    Generate "switch" structure.

    :param condition Condition value.
    If `None` is supplied, a random entity is generated.

    :returns "case" statement item
    """

    if condition is None:
        if bool(random.getrandbits(1)):
            # Entity chain
            src_condition, tar_condition = gen_entity_chain_pair(add_semicolon=False, should_add_mask_indices=False)
        else:
            # Class construct
            src_condition, tar_condition = gen_class_construct_pair(add_semicolon=False,
                                                                    should_add_mask_indices=False)
    else:
        src_condition = condition
        tar_condition = condition

    item_wo_open_bracket = gen_item(f'switch ({src_condition})', f'switch ({tar_condition})')
    item_w_open_bracket = add_open_bracket(item_wo_open_bracket)

    return [
        item_wo_open_bracket,
        item_w_open_bracket
    ]


def __gen_case(condition: str = None):
    """
    Generate switch case.

    :param condition Condition value.
    If `None` is supplied, a random entity chain is generated.

    :returns Switch case item
    """

    if condition is None:
        # Entity chain
        src_condition, tar_condition = gen_entity_chain_pair(add_semicolon=False, should_add_mask_indices=False)
    else:
        src_condition = condition
        tar_condition = condition

    return gen_item(f'case {src_condition}:', f'case {tar_condition}:')


def gen_switch_data(write, switch_count: int, case_count: int):
    """Generate "switch" structures and case statements."""

    # Generate single mask token "switch" structures
    for i in __gen_switch_structs(gen_mask_token(0)):
        write(i)

    # Generate "switch" structures
    for _ in range(switch_count):
        for i in __gen_switch_structs():
            write(i)

    # Generate single mask token switch case
    write(__gen_case(gen_mask_token(0)))

    # Generate switch cases
    for _ in range(case_count):
        write(__gen_case())
