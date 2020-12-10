import random

from theory_data_gen.constants import MASK_TOKEN
from theory_data_gen.utils import join
from .arithmetic import gen_arithmetic


def gen_val_list(entity_chain_callback, mask_token_args_only=False) -> str:
    """Generate a value list."""

    val_range = range(11)
    val_count = random.choices(val_range, weights=(80, 70, 60, 40, 30, 20, 5, 4, 3, 2, 1), k=1)[0]
    vals = []

    # Generate value list
    for i in range(val_count):
        selection = 0 if mask_token_args_only else random.choice(range(5))

        if selection < 3:
            # Mask token
            vals.append(MASK_TOKEN)
        elif selection == 3:
            # Arithmetic
            vals.append(gen_arithmetic())
        elif selection == 4:
            # Entity chain
            vals.append(
                entity_chain_callback(add_semicolon=False, mask_token_args_only=True, should_add_mask_indices=False))

    return join(vals, ', ')
