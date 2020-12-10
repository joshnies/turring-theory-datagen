from tqdm import tqdm

from theory_data_gen.common import gen_mask_token
from .cpp import CPP_PRIM_TYPES


def __gen_for_struct():
    """Generate "for" loop structure."""

    # TODO: Add random "for" expression as condition
    return 'for () {{'


def __gen_foreach_pair(t=None):
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


def __gen_while_struct():
    """Generate "while" loop structure."""

    # TODO: Add random boolean expression as condition
    return 'while () {{'


def __gen_do_struct():
    """Generate "do" structure."""

    return 'do {{'


def gen_loop_structs():
    """Generate loop structures."""

    data = []

    # Generate "for" loop structure
    for_loop = __gen_for_struct()
    item = {'source': for_loop, 'target': for_loop}
    data.append(item)

    # Generate primitive type "foreach" loop structures
    for t in tqdm(CPP_PRIM_TYPES, desc='Generating loop structures (primitive types)'):
        (source, target) = __gen_foreach_pair(t)
        item = {'source': source, 'target': target}
        data.append(item)

    # Generate user type "foreach" loop structures
    (source, target) = __gen_foreach_pair()
    item = {'source': source, 'target': target}
    data.append(item)

    # Generate "while" loop structure
    while_loop = __gen_while_struct()
    item = {'source': while_loop, 'target': while_loop}
    data.append(item)

    # Generate "do" structure
    do_struct = __gen_do_struct()
    item = {'source': do_struct, 'target': do_struct}
    data.append(item)

    return data
