import random

from tqdm import tqdm
from theory_data_gen.common import gen_val_list, gen_mask_token, add_mask_indices
from theory_data_gen.utils import join, join_rand
from .generics import gen_provided_generics


def __gen_entity(mask_index: int):
    """Generates an entity (object or function call) for an entity chain pair."""

    # Generate mask tokens
    m_name = gen_mask_token(mask_index)

    # Generate data
    is_func_call = bool(random.getrandbits(1))
    func_call_markers = ['(', ')'] if is_func_call else ['', '']
    args = gen_val_list() if is_func_call else ''

    # Generate generics
    generics = gen_provided_generics() if is_func_call else ''

    obj = f'{m_name}{generics}{func_call_markers[0]}{args}{func_call_markers[1]}'

    # Index mask tokens
    obj, last_mask_index = add_mask_indices(obj, start_index=mask_index + 1)

    return obj, last_mask_index


def __gen_entity_chain_pair():
    """Generate an entity chain pair."""

    length_range = range(1, 11)
    length = random.choices(length_range, weights=(80, 70, 60, 40, 30, 20, 5, 4, 3, 2), k=1)[0]
    objs = []

    # Generate func args
    next_mask_index = 0

    for i in range(length):
        obj, last_mask_index = __gen_entity(next_mask_index)
        next_mask_index = last_mask_index + 1
        objs.append(obj)

    source_objs = join_rand(objs, ['.', '->', '::'])
    target_objs = join(objs, '.')

    source = f'{source_objs};'
    target = f'{target_objs};'
    return source, target


def gen_entity_chains(count):
    """Generate all entity chains."""

    data = []

    for _ in tqdm(range(count), desc='Generating entity chains'):
        (source, target) = __gen_entity_chain_pair()
        item = {'source': source, 'target': target}
        data.append(item)

    return data