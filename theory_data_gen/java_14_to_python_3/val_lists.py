import random

from theory_data_gen.common import add_mask_indices
from theory_data_gen.constants import MASK_TOKEN
from theory_data_gen.utils import join
from .arithmetic import gen_arithmetic


def gen_val_list(entity_chain_callback, mask_token_args_only=False, should_add_mask_indices=False):
    """Generate a value list."""

    val_range = range(11)
    val_count = random.choices(val_range, weights=(80, 70, 60, 40, 30, 20, 5, 4, 3, 2, 1), k=1)[0]
    source_vals = list()
    target_vals = list()

    # Generate value list
    for i in range(val_count):
        selection = 0 if mask_token_args_only else random.choice(range(6))

        if selection < 3:
            # Mask token
            val = MASK_TOKEN
            source_vals.append(val)
            target_vals.append(val)
        elif selection == 3:
            # Arithmetic
            s, t = gen_arithmetic()
            source_vals.append(s)
            target_vals.append(t)
        elif selection == 4:
            # Entity chain
            s, t = entity_chain_callback(add_semicolon=False, mask_token_args_only=True, should_add_mask_indices=False)
            source_vals.append(s)
            target_vals.append(t)
        else:
            # Null
            source_vals.append('null')
            target_vals.append('None')

    src = join(source_vals, ', ')
    tar = join(source_vals, ', ')

    # Add mask indices
    if should_add_mask_indices:
        src = add_mask_indices(src)
        tar = add_mask_indices(tar)

    return src, tar
