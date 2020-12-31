from tqdm import tqdm

from theory_data_gen.common import gen_item, add_scope_open_token
from theory_data_gen.constants import MASK_TOKEN
from .arithmetic import gen_arithmetic
from .for_loop_inputs import gen_foreach_loop_input_pair


def __gen_foreach_loop_items(src_input: str = None, tar_input: str = None):
    """Generate "foreach"-style loop items."""

    # Generate "for" loop inputs
    if src_input is None or tar_input is None:
        src_input, tar_input = gen_foreach_loop_input_pair()

    # Generate items
    item_wo_open_bracket = gen_item(f'for ({src_input})', f'for {tar_input}:')
    item_w_open_bracket = add_scope_open_token(item_wo_open_bracket, src_only=True)

    return [
        item_wo_open_bracket,
        item_w_open_bracket
    ]


def __gen_while_loop_items():
    """Generate "while" loop items."""

    source_condition, target_condition = gen_arithmetic(only_bool=True)

    item_wo_open_bracket = gen_item(f'while ({source_condition})', f'while {target_condition}:')
    item_w_open_bracket = add_scope_open_token(item_wo_open_bracket, src_only=True)

    return [
        item_wo_open_bracket,
        item_w_open_bracket
    ]


def gen_loops(write, count: int):
    """Generate loops."""

    # Generate "foreach"-style loops with masked inputs
    src_input, tar_input = gen_foreach_loop_input_pair(MASK_TOKEN, MASK_TOKEN)
    for i in __gen_foreach_loop_items(src_input, tar_input):
        write(i)

    # Generate "foreach"-style loops
    for _ in tqdm(range(count), desc='Generating "for" loops'):
        for i in __gen_foreach_loop_items():
            write(i)

    # Generate "while" loops
    for _ in tqdm(range(count), desc='Generating "while" loops'):
        for i in __gen_while_loop_items():
            write(i)
