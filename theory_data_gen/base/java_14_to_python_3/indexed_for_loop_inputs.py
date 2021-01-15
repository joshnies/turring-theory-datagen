import random

from tqdm import tqdm

from theory_data_gen.common import gen_mask_token, add_mask_indices, gen_item
from .arithmetic import gen_arithmetic
from .lvp import TYPE_MAP


def gen_indexed_for_loop_input_pair(use_increment=None):
    """Generate indexed "for" loop input pair."""

    # Generate mask tokens
    type_is_masked = bool(random.getrandbits(1))
    t = gen_mask_token(0) if type_is_masked is None else random.choice(list(TYPE_MAP.keys()))
    base_m_idx = 0 if type_is_masked else -1

    m_iteratee = gen_mask_token(base_m_idx + 1)
    m_iteratee_def_val = gen_mask_token(base_m_idx + 2)
    next_mask_index = base_m_idx + 3

    if use_increment is None:
        use_increment = bool(random.getrandbits(1))

    inc_dec = '++' if use_increment else '--'

    # Generate "for" loop input pair
    source_condition, target = gen_arithmetic(only_bool=True)

    # General source/target
    source = f'{t} {m_iteratee} = {m_iteratee_def_val}; {source_condition}; {m_iteratee}{inc_dec}'

    # Add mask indices
    source, _ = add_mask_indices(source, next_mask_index)
    target, _ = add_mask_indices(target, next_mask_index)

    return source, target, m_iteratee_def_val, m_iteratee, use_increment


def gen_indexed_for_loop_inputs(write, count: int):
    """Generate indexed "for" loop inputs."""

    for _ in tqdm(range(count), desc='Generating indexed "for" loop inputs'):
        # Incrementing
        src, tar = gen_indexed_for_loop_input_pair(use_increment=True)
        item = gen_item(src, tar)
        write(item)

        # Decrementing
        src, tar = gen_indexed_for_loop_input_pair(use_increment=False)
        item = gen_item(src, tar)
        write(item)
