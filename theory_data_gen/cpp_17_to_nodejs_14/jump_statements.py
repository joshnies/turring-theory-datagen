from theory_data_gen.common import gen_item, gen_mask_token


def gen_jump_statements():
    """Generate jump statements (excluding return statements)."""

    break_stmt = 'break;'
    continue_stmt = 'continue;'

    data = [
        gen_item(break_stmt),
        gen_item(continue_stmt)
    ]

    return data
