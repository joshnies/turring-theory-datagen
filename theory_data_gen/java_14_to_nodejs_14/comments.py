from theory_data_gen.common import gen_item, gen_mask_token


def gen_comments(write):
    """Generate comments."""

    m = gen_mask_token(0)

    items = [
        gen_item(f'//{m}'),
        gen_item(f'/*{m}*/')
    ]

    for i in items:
        write(i)
