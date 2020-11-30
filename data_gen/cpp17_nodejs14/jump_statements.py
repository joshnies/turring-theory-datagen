from data_gen.common import gen_item
from data_gen.mask_tokens import AI_VAL


def gen_jump_statements():
    """Generate all jump statement data."""

    break_stmt = 'break;'
    continue_stmt = 'continue;'
    return_stmt = f'return {AI_VAL};'

    data = [
        gen_item(break_stmt),
        gen_item(continue_stmt),
        gen_item(return_stmt)
    ]

    return data
