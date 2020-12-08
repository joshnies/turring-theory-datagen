from tqdm import tqdm

from common import gen_mask_token
from theory_data_gen.cpp17_nodejs14.cpp import CPP_PRIM_TYPES


def gen_for_pair():
    """Generate "for" loop structure pair."""

    # TODO: Add random "for" expression as condition
    return f'for () {{'


def gen_foreach_pair(t=None):
    """Generate "foreach" loop structure pair."""

    # Generate mask tokens
    m_type = gen_mask_token(0) if t is None else t
    base_m_idx = 0 if t is None else -1

    m_iteratee = gen_mask_token(base_m_idx + 1)
    m_iterator = gen_mask_token(base_m_idx + 2)

    # Generate foreach pair
    source = f'for ({m_type} {m_iteratee} : {m_iterator}) {{'
    target = f'for (let {m_iteratee} of {m_iterator}) {{'
    return source, target


def gen_while_pair():
    """Generate "while" loop structure pair."""

    # TODO: Add random boolean expression as condition
    return f'while () {{'


def gen_do_struct():
    """Generate "do" structure pair."""

    return f'do {{'


def gen_loop_structs():
    """Generate all loop structure data."""

    data = []

    # Generate "for" loop structure
    for_loop = gen_for_pair()
    item = {'source': for_loop, 'target': for_loop}
    data.append(item)

    # Generate primitive type "foreach" loop structures
    for t in tqdm(CPP_PRIM_TYPES, desc='Generating loop structures (primitive types)'):
        (source, target) = gen_foreach_pair(t)
        item = {'source': source, 'target': target}
        data.append(item)

    # Generate user type "foreach" loop structures
    (source, target) = gen_foreach_pair()
    item = {'source': source, 'target': target}
    data.append(item)

    # Generate "while" loop structure
    while_loop = gen_while_pair()
    item = {'source': while_loop, 'target': while_loop}
    data.append(item)

    # Generate "do" structure
    do_struct = gen_do_struct()
    item = {'source': do_struct, 'target': do_struct}
    data.append(item)

    return data
