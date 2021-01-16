import random

from tqdm import tqdm

from theory_data_gen.common import add_mask_indices, gen_item, gen_mask_token
from theory_data_gen.common.java import JAVA_PRIM_TYPES_W_MASK, gen_java_generic_type
from theory_data_gen.constants import MASK_TOKEN
from .arithmetic import gen_arithmetic
from .class_constructs import gen_class_construct_pair
from .entity_chains import gen_entity_chain_pair
from .java import gen_modifier_permutations, to_member_items


def __gen_var_items(is_array=False, member=False):
    """Generate variable items."""

    # Generate type declarations
    has_type = bool(random.getrandbits(1))
    is_generic = random.choice(range(10)) == 0
    java_array_size = f'[{MASK_TOKEN}]' if is_array else ''

    src_decl = ''

    if has_type:
        if is_generic:
            src_decl = gen_java_generic_type() + ' '
        else:
            src_decl = random.choice(JAVA_PRIM_TYPES_W_MASK) + ' '

    tar_decl = 'let ' if has_type and not member else ''
    tar_name_mask_index = 1 if src_decl.strip() == MASK_TOKEN else 0

    # Generate default values
    selection = 5 if is_array and has_type else random.choice(range(5))

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
    elif selection == 4:
        # Null
        val = 'null'
        source_def_val = val
        target_def_val = val
    else:
        # Array initialization
        vals = ', '.join([MASK_TOKEN] * random.choice(range(1, 20)))
        source_def_val = f'{{ {vals} }}'
        target_def_val = f'[{vals}]'

    # Generate source/target
    source = f'{src_decl}{MASK_TOKEN}{java_array_size} = {source_def_val};'
    target = f'{tar_decl}{gen_mask_token(tar_name_mask_index)} = {target_def_val};'

    # Add mask indices
    source, _ = add_mask_indices(source)
    target, _ = add_mask_indices(target, tar_name_mask_index + (2 if is_array else 1))

    base_item = gen_item(source, target)

    # Add modifier permutations
    items = gen_modifier_permutations(base_item, include_abstract=False) if has_type else [base_item]

    # Convert to member variable items
    if member:
        items = to_member_items(items)

    return items


def gen_vars(write, standard_count: int, array_count: int):
    """Generate variables."""

    # Generate standard variables
    for _ in tqdm(range(standard_count), desc='Generating variables'):
        items = __gen_var_items(member=False)
        items.extend(__gen_var_items(member=True))

        for i in items:
            write(i)

    # Generate array variables
    for _ in tqdm(range(array_count), desc='Generating array variables'):
        items = __gen_var_items(is_array=True, member=False)
        items.extend(__gen_var_items(is_array=True, member=True))

        for i in items:
            write(i)