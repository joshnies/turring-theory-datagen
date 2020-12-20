import random
from tqdm import tqdm

from theory_data_gen.common import add_mask_indices, gen_item, gen_mask_token
from theory_data_gen.constants import MASK_TOKEN
from .arithmetic import gen_arithmetic
from .class_constructs import gen_class_construct_pair
from .java import JAVA_PRIM_TYPES, gen_java_generic_type
from .entity_chains import gen_entity_chain_pair


def __gen_var(is_array=False):
    """Generate a variable."""

    # Generate type declarations
    types = JAVA_PRIM_TYPES.copy()
    types.append(MASK_TOKEN)

    has_type = bool(random.getrandbits(1))
    is_generic = random.choice(range(10)) == 0
    java_array_size = f'[{MASK_TOKEN}]' if is_array else ''

    src_decl = ''

    if has_type:
        if is_generic:
            src_decl = gen_java_generic_type() + ' '
        else:
            src_decl = random.choice(types) + ' '

    tar_decl = 'let ' if has_type else ''

    # Generate default values
    selection = 4 if is_array and has_type else random.choice(range(4))

    if selection == 0:
        # Mask token
        val = MASK_TOKEN
        source_def_val = val
        target_def_val = val
    elif selection == 1:
        # Arithmetic/boolean expression
        source_def_val, target_def_val = gen_arithmetic()
    elif selection == 2:
        # Entity chain
        source_def_val, target_def_val = gen_entity_chain_pair(add_semicolon=False, should_add_mask_indices=False)
    elif selection == 3:
        # Class construct
        source_def_val, target_def_val = gen_class_construct_pair(add_semicolon=False, should_add_mask_indices=False)
    else:
        # Array initialization
        vals = ', '.join([MASK_TOKEN] * random.choice(range(1, 20)))
        source_def_val = f'{{ {vals} }}'
        target_def_val = f'[{vals}]'

    # Generate source/target
    source = f'{src_decl}{MASK_TOKEN}{java_array_size} = {source_def_val};'
    target = f'{tar_decl}{gen_mask_token(0)} = {target_def_val};'

    # Add mask indices
    source, _ = add_mask_indices(source)
    target, _ = add_mask_indices(target, 2 if is_array else 1)

    return source, target


def gen_vars(standard_count: int, array_count: int):
    """Generate variables."""

    data = []

    # Generate standard variables
    for _ in tqdm(range(standard_count), desc='Generating variables'):
        source, target = __gen_var()
        data.append(gen_item(source, target))

    # Generate array variables
    for _ in tqdm(range(array_count), desc='Generating array variables'):
        source, target = __gen_var(is_array=True)
        data.append(gen_item(source, target))

    return data