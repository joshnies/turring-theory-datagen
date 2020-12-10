import random
from tqdm import tqdm

from theory_data_gen.common import gen_val_list, add_mask_indices, gen_item
from theory_data_gen.utils import join, join_rand
from theory_data_gen.constants import MASK_TOKEN
from .cpp import CPP_CHAIN_OPS
from .generics import gen_provided_generics


def __gen_entity():
    """Generates an entity (object or function call) for an entity chain pair."""

    # Generate data
    is_func_call = bool(random.getrandbits(1))
    func_call_markers = ['(', ')'] if is_func_call else ['', '']
    args = gen_val_list() if is_func_call else ''

    # Generate generics
    generics = gen_provided_generics() if is_func_call else ''

    obj = f'{MASK_TOKEN}{generics}{func_call_markers[0]}{args}{func_call_markers[1]}'

    return obj


def gen_entity_chain_pair():
    """Generate an entity chain pair."""

    entities = []
    length_range = range(1, 11)
    length = random.choices(length_range, weights=(80, 70, 60, 40, 30, 20, 5, 4, 3, 2), k=1)[0]

    # Generate entities
    for i in range(length):
        entities.append(__gen_entity())

    source_entities = join_rand(entities, CPP_CHAIN_OPS)
    source_entities, _ = add_mask_indices(source_entities)

    target_entities = join(entities, '.')
    target_entities, _ = add_mask_indices(target_entities)

    source = f'{source_entities};'
    target = f'{target_entities};'
    return source, target


def gen_entity_chains(count):
    """Generate entity chains."""

    data = []

    for _ in tqdm(range(count), desc='Generating entity chains'):
        (source, target) = gen_entity_chain_pair()
        data.append(gen_item(source, target))

    return data
