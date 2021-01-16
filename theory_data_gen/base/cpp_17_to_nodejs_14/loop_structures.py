import random

from tqdm import tqdm

from theory_data_gen.common import gen_mask_token, gen_item, add_scope_open_token
from .arithmetic import gen_arithmetic
from .cpp import CPP_PRIM_TYPES
from .for_loop_inputs import gen_for_loop_input_pair


def __gen_for_loop_items():
    """Generate "for" loop items."""

    source_input, target_input = gen_for_loop_input_pair(bool(random.getrandbits(1)))

    item_wo_open_bracket = gen_item(f'for ({source_input})', f'for ({target_input})')
    item_w_open_bracket = add_scope_open_token(item_wo_open_bracket)

    return [
        item_wo_open_bracket,
        item_w_open_bracket
    ]


def __gen_foreach_items(t=None):
    """Generate "foreach" loop items."""

    # Generate mask tokens
    m_type = gen_mask_token(0) if t is None else t
    base_m_idx = 0 if t is None else -1

    m_iteratee = gen_mask_token(base_m_idx + 1)
    m_iterator = gen_mask_token(base_m_idx + 2)

    # Generate foreach pairs
    item_wo_open_bracket = gen_item(f'for ({m_type} {m_iteratee} : {m_iterator})',
                                    f'for (let {m_iteratee} of {m_iterator})')

    item_w_open_bracket = add_scope_open_token(item_wo_open_bracket)

    return [
        item_wo_open_bracket,
        item_w_open_bracket
    ]


def __gen_while_loop_items(source_condition: str = None, target_condition: str = None):
    """Generate "while" loop items."""

    if source_condition is None and target_condition is None:
        source_condition, target_condition = gen_arithmetic(only_bool=True)

    item_wo_open_bracket = gen_item(f'while ({source_condition})', f'while ({target_condition})')
    item_w_open_bracket = add_scope_open_token(item_wo_open_bracket)

    return [
        item_wo_open_bracket,
        item_w_open_bracket
    ]


def gen_loops(write, count: int):
    """
    Generate loops.

    :param write: Function that, when called, writes the given data to the output file.
    :param count: Number of loops to generate for each type.
    """

    # Generate "for" loops
    for _ in tqdm(range(count), desc='Generating "for" loops'):
        for i in __gen_for_loop_items():
            write(i)

    # Generate primitive type "foreach" loop structures
    for t in tqdm(CPP_PRIM_TYPES, desc='Generating "foreach" loops'):
        for i in __gen_foreach_items(t):
            write(i)

    # Generate user type "foreach" loop structures
    for i in __gen_foreach_items():
        write(i)

    # Generate "while" loops
    for _ in tqdm(range(count), desc='Generating "while" loops'):
        for i in __gen_while_loop_items():
            write(i)

    # Generate bool constant "while" loops
    for i in __gen_while_loop_items('true', 'true'):
        write(i)

    for i in __gen_while_loop_items('false', 'false'):
        write(i)