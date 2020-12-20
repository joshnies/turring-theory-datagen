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


def __gen_else_struct():
    """Generate "else" structure."""

    return 'else {'


def gen_conditional_structs(count: int):
    """Generate conditional structures."""

    data = []

    # Generate "if" structure
    for _ in tqdm(range(count), desc='Generating "if" structures'):
        source, target = __gen_if_pair()
        data.append(gen_item(source, target))

    # Generate "else if" structure
    for _ in tqdm(range(count), desc='Generating "else if" structures'):
        source, target = __gen_if_pair(has_else=True)
        data.append(gen_item(source, target))

    # Generate "else" structure
    item = gen_item(__gen_else_struct())
    data.append(item)

    return data
