import random

from theory_data_gen.common import gen_item, add_mask_indices, gen_mask_token
from theory_data_gen.constants import MASK_TOKEN


def __gen_var_type():
    """Generate variable type."""

    choice = random.choice(range(0, 5))

    # we really don't care about the range size unless it's 18 - then it's a 'long'
    int_arg_range = [3, 4, 9, 18]
    int_arg_count = random.choices(int_arg_range, weights=(10, 40, 40, 10), k=1)[0]

    if choice == 0:
        # Generate alphanumeric type
        str_arg_range = range(1, 100)
        return f'X({str_arg_range})'
    elif choice == 1:
        # Generate numeric
        return f'9({int_arg_count})'
    elif choice == 2:
        # Generate char
        return f'N'
    elif choice == 3:
        # Generate alphabetic
        return f'A({int_arg_count})'
    elif choice == 4:
        # Generate float
        rand_decimal_places = range(1, 9)
        return f' S9({int_arg_count})V9{rand_decimal_places}'
    else:
        # Generate signed numeric
        return f'S9({int_arg_count})'


def __gen_var_items():
    """Generate variable items."""

    # Generate source/target
    # 01 VarName PIC <type>.
    source = f'{MASK_TOKEN} {MASK_TOKEN} PIC {__gen_var_type()}.'
    # TODO - how do we know if private/public?
    # TODO - what index used with gen_mask_token?
    # TODO - how do we handle default vals
    target = f'private {gen_mask_token(0)} todo'

    item = gen_item(source, target)
    item, _ = add_mask_indices(item)

    return [item]


def gen_vars(write, count):
    """Generate variables."""

    for _ in range(count):
        for i in __gen_var_items():
            write(i)
