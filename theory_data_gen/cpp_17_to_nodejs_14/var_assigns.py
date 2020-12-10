import random
from tqdm import tqdm

from theory_data_gen.common import add_mask_indices, gen_item
from theory_data_gen.constants import MASK_TOKEN
from .arithmetic import gen_arithmetic
from .entity_chains import gen_entity_chain_pair


def __gen_var_assign():
    """Generate a variable assignment."""

    source_def_val = None
    target_def_val = None

    selection = random.choice(range(3))

    if selection == 0:
        # Mask token
        def_val = MASK_TOKEN
    elif selection == 1:
        # Arithmetic/boolean expression
        source_def_val, target_def_val = gen_arithmetic()
    else:
        # Entity chain
        source_def_val, target_def_val = gen_entity_chain_pair(add_semicolon=False, should_add_mask_indices=False)

    # Generate source/target
    source = f'{MASK_TOKEN} = {source_def_val};'
    target = f'{MASK_TOKEN} = {target_def_val};'

    # Add mask indices
    source, _ = add_mask_indices(source)
    target, _ = add_mask_indices(target)

    return source, target


def gen_var_assigns(count: int):
    """Generate variable assignments."""

    data = []

    for _ in tqdm(range(count), desc='Generating variable assignments'):
        source, target = __gen_var_assign()
        data.append(gen_item(source, target))

    return data
