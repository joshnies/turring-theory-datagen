from theory_data_gen.common import gen_item, gen_mask_token


def gen_rogue_entities():
    """Generate rogue entities."""

    return [
        gen_item('{'),
        gen_item('}'),
        gen_item('};'),
        gen_item('('),
        gen_item(')'),
        gen_item(');'),
        gen_item(';'),
        gen_item(f'{gen_mask_token(0)},'),
        gen_item(f'{gen_mask_token(0)};'),
        gen_item(f'{gen_mask_token(0)})'),
        gen_item(f'{gen_mask_token(0)}}}')
    ]
