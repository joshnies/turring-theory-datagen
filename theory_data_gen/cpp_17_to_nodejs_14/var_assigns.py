from common import gen_mask_token


def gen_var_assigns():
    """Generate a variable assignment pair."""

    m_0 = gen_mask_token(0)
    m_1 = gen_mask_token(1)

    assign = f'{m_0} = {m_1};'
    return [{'source': assign, 'target': assign}]