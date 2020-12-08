from theory_data_gen.common import gen_item, gen_mask_token


def gen_jump_statements():
    """Generate all jump statement data."""

    break_stmt = 'break;'
    continue_stmt = 'continue;'

    # TODO: Add support for returned arithmetic expressions (reused)
    # TODO: Add support for returned boolean expressions (reused)
    # TODO: Add support for returned entity chains (reused)
    return_stmt = f'return {gen_mask_token(0)};'

    data = [
        gen_item(break_stmt),
        gen_item(continue_stmt),
        gen_item(return_stmt)
    ]

    return data
