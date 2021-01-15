from tqdm import tqdm

from theory_data_gen.common import gen_item
from .arithmetic import gen_arithmetic


def __gen_if_item(has_else=False):
    """Generate "if" structure item."""

    source_condition, target_condition = gen_arithmetic(should_add_mask_indices=True, only_bool=True)
    else_token = 'else ' if has_else else ''

    return gen_item(
        f'{else_token}if ({source_condition}) {{',
        f'{else_token}if ({target_condition}) {{'
    )


def __gen_else_item():
    """Generate "else" structure item."""

    return gen_item('else {')


def gen_conditional_structs(write, count: int):
    """Generate conditional structures."""

    # Generate "if" structure
    for _ in tqdm(range(count), desc='Generating "if" structures'):
        write(__gen_if_item())

    # Generate "else if" structure
    for _ in tqdm(range(count), desc='Generating "else if" structures'):
        write(__gen_if_item(has_else=True))

    # Generate "else" structure
    write(__gen_else_item())
