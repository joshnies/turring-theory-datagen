from tqdm import tqdm

from common import gen_mask_token
from theory_data_gen.mask_tokens import AI_CONDITION
from theory_data_gen.cpp_17_to_nodejs_14.cpp import CPP_PRIM_TYPES


def gen_for_loop_input_pair(t=None, use_increment=False):
    """Generate "for" loop input pair."""

    # Generate mask tokens
    m_type = gen_mask_token(0) if t is None else t
    base_m_idx = 0 if t is None else -1

    m_iteratee = gen_mask_token(base_m_idx + 1)
    m_iteratee_def_val = gen_mask_token(base_m_idx + 2)

    inc_dec = '++' if use_increment else '--'

    # Generate "for" loop input pair
    # TODO: Replace "AI_CONDITION" with a random boolean expression
    source = f'{m_type} {m_iteratee} = {m_iteratee_def_val}; {AI_CONDITION}; {m_iteratee}{inc_dec}'
    target = f'let {m_iteratee} = {m_iteratee_def_val}; {AI_CONDITION}; {m_iteratee}{inc_dec}'
    return source, target


def gen_for_loop_inputs():
    """Generate all "for" loop inputs."""

    data = []

    # Generate "for" loop input pairs for pre-defined types
    for t in tqdm(CPP_PRIM_TYPES, desc='Generating "for" loop inputs'):
        (source, target) = gen_for_loop_input_pair(t)
        item = {'source': source, 'target': target}
        data.append(item)

    # Generate "for" loop input pair for user type
    (source, target) = gen_for_loop_input_pair()
    item = {'source': source, 'target': target}
    data.append(item)

    return data
