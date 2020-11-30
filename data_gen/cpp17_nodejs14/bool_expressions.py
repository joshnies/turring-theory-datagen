from data_gen.common import gen_item
from data_gen.mask_tokens import AI_EXTRACTION


def gen_bool_expressions():
    """Generate boolean expressions."""

    data = []

    ops_map = {
        '==': '===',
        '!=': '!=='
    }

    same_ops = ['>=', '<=', '&&', '||']

    for src_op in ops_map:
        data.append(
            gen_item(f'{AI_EXTRACTION} {src_op} {AI_EXTRACTION}', f'{AI_EXTRACTION} {ops_map[src_op]} {AI_EXTRACTION}'))

    for op in same_ops:
        data.append(gen_item(f'{AI_EXTRACTION} {op} {AI_EXTRACTION}', f'{AI_EXTRACTION} {op} {AI_EXTRACTION}'))

    return data
