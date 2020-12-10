from theory_data_gen.common import gen_item


def gen_rogue_entities():
    """Generate rogue entities."""

    return [
        gen_item('{'),
        gen_item('}'),
        gen_item('};'),
        gen_item('('),
        gen_item(')'),
        gen_item(');'),
        gen_item(';')
    ]
