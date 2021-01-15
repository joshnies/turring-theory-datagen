from tqdm import tqdm

from theory_data_gen.common import gen_item
from .arithmetic import gen_arithmetic


def __gen_if_pair(has_else=False):
    """Generate "if" structure."""

    source_condition, target_condition = gen_arithmetic(should_add_mask_indices=True, only_bool=True)
    src_token = 'else if ' if has_else else 'if '
    tar_token = 'elif ' if has_else else 'if '

    return gen_item(
        f'{src_token} ({source_condition}) {{',
        f'{tar_token} {target_condition}:'
    )


def gen_conditional_structs(write, count: int):
    """Generate conditional structures."""

    # Generate "if" structure
    for _ in tqdm(range(count), desc='Generating "if" structures'):
        # "if"
        write(__gen_if_pair())

        # "else if"
        write(__gen_if_pair(has_else=True))
