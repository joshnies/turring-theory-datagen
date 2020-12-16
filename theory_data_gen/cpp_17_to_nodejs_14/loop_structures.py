import random
from tqdm import tqdm

from theory_data_gen.common import gen_mask_token, gen_item
from .arithmetic import gen_arithmetic
from .cpp import CPP_PRIM_TYPES
from .for_loop_inputs import gen_for_loop_input_pair


def __gen_for_loop_pair():
    """Generate "for" loop pair."""

    source_input, target_input = gen_for_loop_input_pair(bool(random.getrandbits(1)))

    source = f'for ({source_input}) {{'
    target = f'for ({target_input}) {{'

    return source, target


def __gen_foreach_pair(t=None):
    """Generate "foreach" loop pair."""

    # Generate mask tokens
    m_type = gen_mask_token(0) if t is None else t
    base_m_idx = 0 if t is None else -1

    m_iteratee = gen_mask_token(base_m_idx + 1)
    m_iterator = gen_mask_token(base_m_idx + 2)

    # Generate foreach pair
    source = f'for ({m_type} {m_iteratee} : {m_iterator}) {{'
    target = f'for (let {m_iteratee} of {m_iterator}) {{'
    return source, target


def __gen_while_loop_pair():
    """Generate "while" loop pair."""

    source_condition, target_condition = gen_arithmetic(only_bool=True)

    source = f'while ({source_condition}) {{'
    target = f'while ({target_condition}) {{'

    return source, target


def __gen_do_struct():
    """Generate "do" structure."""

    return 'do {'


def gen_loops(count: int):
    """Generate loops."""

    data = []

    # Generate "for" loops
    for _ in tqdm(range(count), desc='Generating "for" loops'):
        source, target = __gen_for_loop_pair()
        data.append(gen_item(source, target))

    # Generate primitive type "foreach" loop structures
    for t in tqdm(CPP_PRIM_TYPES, desc='Generating "foreach" loops'):
        (source, target) = __gen_foreach_pair(t)
        item = {'source': source, 'target': target}
        data.append(item)

    # Generate user type "foreach" loop structures
    (source, target) = __gen_foreach_pair()
    item = {'source': source, 'target': target}
    data.append(item)

    # Generate "while" loops
    for _ in tqdm(range(count), desc='Generating "while" loops'):
        source, target = __gen_while_loop_pair()
        data.append(gen_item(source, target))

    # Generate "do" structure
    do_struct = __gen_do_struct()
    item = {'source': do_struct, 'target': do_struct}
    data.append(item)

    return data
