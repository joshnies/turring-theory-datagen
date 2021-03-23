import random

from tqdm import tqdm

from ...constants import MASK_TOKEN
from ...common import gen_item, gen_mask_token, add_mask_indices
from ...common.cobol import COBOL_BOOL_OP_MAP, COBOL_SIGN_OP_MAP, COBOL_EQUALITY_OPS

SRC_SUBVALUE = f'({MASK_TOKEN})'
TAR_SUBVALUE = f'.GetSubvalue(start: {MASK_TOKEN})'


def __gen_relation_condition():
    """Generate relation condition pair."""

    # Get operators
    src_op = random.choice(list(COBOL_BOOL_OP_MAP.keys()))
    tar_op = COBOL_BOOL_OP_MAP[src_op]

    # Generate random keywords and clauses
    src_is = 'IS ' if bool(random.getrandbits(1)) else ''

    src_subvalue_a = ''
    src_subvalue_b = ''
    tar_subvalue_a = ''
    tar_subvalue_b = ''

    # Generate value
    val_choice = random.randint(0, 3)

    if val_choice == 0:
        # Mask token
        src_val = MASK_TOKEN
        tar_val = MASK_TOKEN

        has_subvalue_a = bool(random.getrandbits(1))
        has_subvalue_b = bool(random.getrandbits(1))

        src_subvalue_a = SRC_SUBVALUE if has_subvalue_a else ''
        src_subvalue_b = SRC_SUBVALUE if has_subvalue_a else ''
        tar_subvalue_a = TAR_SUBVALUE if has_subvalue_b else ''
        tar_subvalue_b = TAR_SUBVALUE if has_subvalue_b else ''
    elif val_choice == 1:
        # Null
        src_val = 'NULL'
        tar_val = 'null'

        # Ensure operator is compatible
        src_op = random.choice(COBOL_EQUALITY_OPS)
        tar_op = COBOL_BOOL_OP_MAP[src_op]
    elif val_choice == 2:
        # ZERO constant
        is_plural = bool(random.getrandbits(1))
        plural = 'S' if is_plural else ''
        extra_e = 'E' if is_plural and bool(random.getrandbits(1)) else ''
        src_val = f'ZERO{extra_e}{plural}'
        tar_val = '0'
    else:
        # SPACE constant
        plural = 'S' if bool(random.getrandbits(1)) else ''
        src_val = f'SPACE{plural}'
        tar_val = '" "'

        # Ensure operator is compatible
        src_op = random.choice(COBOL_EQUALITY_OPS)
        tar_op = COBOL_BOOL_OP_MAP[src_op]

    # Generate pair
    src = f'{MASK_TOKEN}{src_subvalue_a} {src_is}{src_op} {src_val}{src_subvalue_b}'
    tar = f'{MASK_TOKEN}{tar_subvalue_a} {tar_op} {tar_val}{tar_subvalue_b}'

    return src, tar


def __gen_sign_condition():
    """Generate sign condition pair."""

    # Get operators
    src_op = random.choice(list(COBOL_SIGN_OP_MAP.keys()))
    tar_op = COBOL_SIGN_OP_MAP[src_op]

    src_is = 'IS ' if bool(random.getrandbits(1)) else ''

    # Generate
    src = f'{MASK_TOKEN} {src_is}{src_op}'
    tar = f'{MASK_TOKEN} {tar_op}'

    return src, tar


def __gen_class_condition():
    """Generate class condition pair."""

    # Generate pair
    type_selection = random.choice(range(6))
    negate = bool(random.getrandbits(1))

    src_is = 'IS ' if bool(random.getrandbits(1)) else ''
    src_negate = 'NOT ' if negate else ''
    tar_negate = '!' if negate else ''

    has_subvalue = bool(random.getrandbits(1))
    src_subvalue = SRC_SUBVALUE if has_subvalue else ''
    tar_subvalue = TAR_SUBVALUE if has_subvalue else ''

    if type_selection == 0:
        # Numeric
        src = f'{MASK_TOKEN}{src_subvalue} {src_is}{src_negate}NUMERIC'
        tar = f'{tar_negate}{MASK_TOKEN}{tar_subvalue}.IsNumeric()'
    elif type_selection == 1:
        # Alphabetic
        src = f'{MASK_TOKEN}{src_subvalue} {src_is}{src_negate}ALPHABETIC'
        tar = f'{tar_negate}{MASK_TOKEN}{tar_subvalue}.IsAlphabetic()'
    elif type_selection == 2:
        # Alphanumeric
        src = f'{MASK_TOKEN}{src_subvalue} {src_is}{src_negate}ALPHANUMERIC'
        tar = f'{tar_negate}{MASK_TOKEN}{tar_subvalue}.IsAlphanumeric()'
    elif type_selection == 3:
        # Alphabetic (lowercase)
        src = f'{MASK_TOKEN}{src_subvalue} {src_is}{src_negate}ALPHABETIC-LOWER'
        tar = f'{tar_negate}{MASK_TOKEN}{tar_subvalue}.IsAlphabeticLower()'
    else:
        # Alphabetic (uppercase)
        src = f'{MASK_TOKEN}{src_subvalue} {src_is}{src_negate}ALPHABETIC-UPPER'
        tar = f'{tar_negate}{MASK_TOKEN}{tar_subvalue}.IsAlphabeticUpper()'

    return src, tar


def __gen_name_condition():
    """Generate name condition pair."""

    is_negated = bool(random.getrandbits(1))
    src_not = 'NOT ' if is_negated else ''
    tar_not = '!' if is_negated else ''

    has_subvalue = bool(random.getrandbits(1))
    src_subvalue = SRC_SUBVALUE if has_subvalue else ''
    tar_subvalue = TAR_SUBVALUE if has_subvalue else ''

    # Generate positive condition
    src = f'{src_not}{MASK_TOKEN}{src_subvalue}'
    tar = f'{tar_not}{MASK_TOKEN}{tar_subvalue}'

    return src, tar


def gen_condition():
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
        c = gen_condition()
        all_src_conditions.append(c[0])
        all_tar_conditions.append(c[1])

    # Generate base item
    src_condition = ' AND '.join(all_src_conditions)
    tar_condition = ' && '.join(all_tar_conditions)

    src = f'IF {src_condition}'
    src, _ = add_mask_indices(src)

    tar = f'if ({tar_condition}) {{'
    tar, _ = add_mask_indices(tar)

    items.append(gen_item(src, tar))

    # Generate "else if" item
    src = 'ELSE-' + src
    tar = '} else ' + tar
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
            f'IF {gen_mask_token(0)}',
            f'if ({gen_mask_token(0)}) {{'
        ),
        (
            f'ELSE-IF {gen_mask_token(0)}',
            f'else if ({gen_mask_token(0)}) {{'
        ),
        (
            f'IF NOT {gen_mask_token(0)}',
            f'if (!{gen_mask_token(0)}) {{'
        ),
        (
            f'ELSE-IF NOT {gen_mask_token(0)}',
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
