from tqdm import tqdm

from theory_data_gen.common import gen_item
from .arithmetic import gen_arithmetic


def __gen_if_pair(has_else=False):
    """Generate "if" structure."""

    source_condition, target_condition = gen_arithmetic(should_add_mask_indices=True, only_bool=True)
    else_token = 'else ' if has_else else ''

    source = f'{else_token}if ({source_condition}) {{'
    target = f'{else_token}if ({target_condition}) {{'

    return source, target


def gen_conditional_structs(write, count: int):
    """Generate conditional structures."""

    # Generate "if" structure
    for _ in tqdm(range(count), desc='Generating "if" structures'):
        src, tar = __gen_if_pair()
        item = gen_item(src, tar)
        write(item)

    # Generate "else if" structure
    for _ in tqdm(range(count), desc='Generating "else if" structures'):
        src, tar = __gen_if_pair(has_else=True)
        item = gen_item(src, tar)
        write(item)
