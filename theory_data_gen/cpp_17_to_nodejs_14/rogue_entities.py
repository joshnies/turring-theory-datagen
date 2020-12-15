from theory_data_gen.common import gen_item, gen_mask_token


def gen_rogue_entities():
    """Generate rogue entities."""

    m_0 = gen_mask_token(0)

    return [
        gen_item('{'),
        gen_item('}'),
        gen_item('};'),
        gen_item('('),
        gen_item(')'),
        gen_item(');'),
        gen_item(';'),
        # gen_item(m_0),
        # gen_item(f'{m_0},'),
        # gen_item(f'{m_0};'),
        # gen_item(f'{m_0})'),
        # gen_item(f'{m_0}}}'),
        gen_item(f'{m_0}++'),
        gen_item(f'{m_0}++;'),
        gen_item(f'{m_0}--'),
        gen_item(f'{m_0}--;')
    ]
