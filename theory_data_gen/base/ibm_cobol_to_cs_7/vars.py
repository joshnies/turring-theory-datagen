import random

from theory_data_gen.constants import MASK_TOKEN

def __gen_var_type():
    """Generate variable type."""

    choice = random.choice(range(0, 3))

    if choice == 0:
        # Generate alphanumeric type
        return f'X({<RANDOM_NUM>})'
    elif choice == 1:
        # Generate ....
    else:
        # Generate ...

def __gen_var_items():
    """Generate variable items."""

    # 01 VarName PIC <type>.
    src = f'{MASK_TOKEN} {MASK_TOKEN} PIC {__gen_var_type()}.'

def gen_vars(write, count):
    """Generate variables."""

    for _ in range(count):
        for i in __gen_var_items():
            write(i)
