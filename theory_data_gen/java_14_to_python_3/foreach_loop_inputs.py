import random

from tqdm import tqdm

from constants import MASK_TOKEN
from theory_data_gen.common import gen_mask_token, add_mask_indices, gen_item
from .entity_chains import gen_entity_chain_pair
from .lvp import TYPE_MAP


def gen_foreach_loop_input_pair(src_input: str = None, tar_input: str = None):
    """Generate "foreach"-style loop input pair."""

    # Generate mask tokens
    type_is_masked = bool(random.getrandbits(1))
    src_type = gen_mask_token(0) if type_is_masked is None else random.choice(list(TYPE_MAP.keys()))
    base_m_idx = 0 if type_is_masked else -1

    m_iteratee = gen_mask_token(base_m_idx + 1)
    next_mask_index = base_m_idx + 3

    # Generate input
    if src_input is None or tar_input is None:
        src_input, tar_input = gen_entity_chain_pair(add_semicolon=False, should_add_mask_indices=False)

    # General source/target
    source = f'{src_type} {m_iteratee} : {src_input}'
    target = f'{m_iteratee} in {tar_input}'

    # Add mask indices
    source, _ = add_mask_indices(source, next_mask_index)
    target, _ = add_mask_indices(target, next_mask_index)

    return source, target


def gen_foreach_loop_inputs(write, count: int):
    """Generate "foreach"-style loop inputs."""

    # Generate with masked inputs
    src, tar = gen_foreach_loop_input_pair(MASK_TOKEN, MASK_TOKEN)
    item = gen_item(src, tar)
    write(item)

    # Generate with random entity chain inputs
    for _ in tqdm(range(count), desc='Generating "foreach"-style loop inputs'):
        src, tar = gen_foreach_loop_input_pair()
        item = gen_item(src, tar)
        write(item)
