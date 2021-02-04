import random

from tqdm import tqdm

from ...constants import MASK_TOKEN
from ...common import gen_item, gen_mask_token, add_mask_indices
from ...common.cobol import COBOL_BOOL_OP_MAP, COBOL_SIGN_OP_MAP


def __gen_relation_condition():
    """Generate relation condition pair."""

    # Get operators
    src_op = random.choice(list(COBOL_BOOL_OP_MAP.keys()))
    tar_op = COBOL_BOOL_OP_MAP[src_op]

    # Generate pair
    src = f'{MASK_TOKEN} {src_op} {MASK_TOKEN}'
    tar = f'{MASK_TOKEN} {tar_op} {MASK_TOKEN}'

    return src, tar


def __gen_sign_condition():
    """Generate sign condition pair."""

    # Get operators
    src_op = random.choice(list(COBOL_SIGN_OP_MAP.keys()))
    tar_op = COBOL_SIGN_OP_MAP[src_op]

    src_is = 'IS ' if bool(random.getrandbits(1)) else ''

    # Generate without "IS"
    src = f'{MASK_TOKEN} {src_is}{src_op} {MASK_TOKEN}'
    tar = f'{MASK_TOKEN} {tar_op} {MASK_TOKEN}'

    return src, tar


def __gen_class_condition():
    """Generate class condition pair."""

    # Generate pair
    type_selection = random.choice(range(6))

    if type_selection == 0:
        # Numeric
        src = f'{MASK_TOKEN} IS NUMERIC'
        tar = f'{MASK_TOKEN}.IsNumeric()'
    elif type_selection == 1:
        # Alphabetic
        src = f'{MASK_TOKEN} IS ALPHABETIC'
        tar = f'{MASK_TOKEN}.IsAlphabetic()'
    elif type_selection == 2:
        # Alphanumeric
        src = f'{MASK_TOKEN} IS ALPHANUMERIC'
        tar = f'{MASK_TOKEN}.IsAlphanumeric()'
    elif type_selection == 3:
        # Alphabetic (lowercase)
        src = f'{MASK_TOKEN} IS ALPHABETIC-LOWER'
        tar = f'{MASK_TOKEN}.IsAlphabeticLower()'
    else:
        # Alphabetic (uppercase)
        src = f'{MASK_TOKEN} IS ALPHABETIC-UPPER'
        tar = f'{MASK_TOKEN}.IsAlphabeticUpper()'

    return src, tar


def __gen_name_condition():
    """Generate name condition pair."""

    is_negated = bool(random.getrandbits(1))
    src_not = 'NOT ' if is_negated else ''
    tar_not = '!' if is_negated else ''

    # Generate positive condition
    src = f'{src_not}{MASK_TOKEN}'
    tar = f'{tar_not}{MASK_TOKEN}'

    return src, tar


def __gen_condition():
    """Generate condition pair."""

    # Generate condition
    condition_type = random.choice(range(4))

    if condition_type == 0:
        # Relation condition
        return __gen_relation_condition()
    elif condition_type == 1:
        # Sign condition
        return __gen_sign_condition()
    elif condition_type == 2:
        # Class condition
        return __gen_class_condition()
    else:
        # Condition-name condition (single variable boolean)
        # (includes negated version)
        return __gen_name_condition()


def __gen_conditional():
    """Generate a conditional item."""

    items = list()

    # Generate conditions
    all_src_conditions = list()
    all_tar_conditions = list()

    for _ in range(random.choice(range(1, 6))):
        c = __gen_condition()
        all_src_conditions.append(c[0])
        all_tar_conditions.append(c[1])

    # Generate base item
    src_condition = ' AND '.join(all_src_conditions)
    tar_condition = ' && '.join(all_tar_conditions)

    src = f'IF {src_condition} THEN'
    src, _ = add_mask_indices(src)

    tar = f'if ({tar_condition}) {{'
    tar, _ = add_mask_indices(tar)

    items.append(gen_item(src, tar))

    # Generate "else if" item
    src = 'ELSE-' + src
    tar = 'else ' + tar
    items.append(gen_item(src, tar))

    return items


def gen_conditionals(write, count: int):
    """
    Generate conditional statements.

    :param write: CSV output write function.
    :param count: Number of conditional statements to generate.
    """

    items = [
        (
            f'IF {gen_mask_token(0)} THEN',
            f'if ({gen_mask_token(0)}) {{'
        ),
        (
            f'ELSE-IF {gen_mask_token(0)} THEN',
            f'else if ({gen_mask_token(0)}) {{'
        ),
        (
            f'IF NOT {gen_mask_token(0)} THEN',
            f'if (!{gen_mask_token(0)}) {{'
        ),
        (
            f'ELSE-IF NOT {gen_mask_token(0)} THEN',
            f'else if (!{gen_mask_token(0)}) {{'
        ),
    ]

    items = list(map(lambda item: gen_item(item[0], item[1]), items))

    # Generate items
    for _ in tqdm(range(count), desc='Generating conditionals'):
        items.extend(__gen_conditional())

    # Write items to output
    for i in items:
        write(i)
