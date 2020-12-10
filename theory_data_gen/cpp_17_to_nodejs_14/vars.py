import random
from tqdm import tqdm

from theory_data_gen.common import add_mask_indices, gen_item
from theory_data_gen.constants import MASK_TOKEN
from .arithmetic import gen_arithmetic
from .cpp import CPP_PRIM_TYPES
from .entity_chains import gen_entity_chain_pair


def __gen_var():
    """Generate a variable."""

    # Generate type declarations
    types = CPP_PRIM_TYPES.copy()
    types.append(MASK_TOKEN)

    has_type = bool(random.getrandbits(1))
    src_decl = random.choice(types) + ' ' if has_type else ''
    tar_decl = 'let ' if has_type else ''

    # Generate default values
    selection = random.choice(range(3))

    if selection == 0:
        # Mask token
        val = MASK_TOKEN
        source_def_val = val
        target_def_val = val
    elif selection == 1:
        # Arithmetic/boolean expression
        source_def_val, target_def_val = gen_arithmetic()
    else:
        # Entity chain
        source_def_val, target_def_val = gen_entity_chain_pair(add_semicolon=False, should_add_mask_indices=False)

    # Generate source/target
    source = f'{src_decl}{MASK_TOKEN} = {source_def_val};'
    target = f'{tar_decl}{MASK_TOKEN} = {target_def_val};'

    # Add mask indices
    source, _ = add_mask_indices(source)
    target, _ = add_mask_indices(target)

    return source, target


def gen_vars(count: int):
    """Generate variables."""

    data = []

    for _ in tqdm(range(count), desc='Generating variables'):
        source, target = __gen_var()
        data.append(gen_item(source, target))

    return data
