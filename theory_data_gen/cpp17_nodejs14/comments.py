from theory_data_gen.common import gen_item, gen_mask_token


def gen_comments():
    """Generate all comment data."""

    m = gen_mask_token(0)

    data = [
        gen_item(f'//{m}'),
        gen_item(f'/*{m}*/')
    ]

    return data
