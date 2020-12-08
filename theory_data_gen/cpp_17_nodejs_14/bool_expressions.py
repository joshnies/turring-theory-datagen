from theory_data_gen.common import gen_item, gen_mask_token


def gen_bool_expressions():
    """Generate boolean expressions."""

    data = []

    ops_map = {
        '==': '===',
        '!=': '!=='
    }

    same_ops = ['>', '<', '>=', '<=', '&&', '||']

    # TODO: Implement arithmetic expressions (reused)
    # TODO: Implement entity chains (reused)
    m_0 = gen_mask_token(0)
    m_1 = gen_mask_token(1)

    for src_op in ops_map:
        data.append(
            gen_item(
                f'{m_0} {src_op} {m_1}',
                f'{m_0} {ops_map[src_op]} {m_1}'
            )
        )

    for op in same_ops:
        data.append(gen_item(f'{m_0} {op} {m_1}'))

    return data
