import random
from tqdm import tqdm

from theory_data_gen.common import add_mask_indices, gen_item
from theory_data_gen.utils import join, join_rand
from theory_data_gen.constants import MASK_TOKEN
from .cpp import CPP_CHAIN_OPS
from .generics import gen_provided_generics
from .val_lists import gen_val_list


def __gen_entity(mask_token_args_only=False):
    """Generates an entity (object or function call) for an entity chain pair."""

    # Generate data
    is_func_call = bool(random.getrandbits(1))
    func_call_markers = ['(', ')'] if is_func_call else ['', '']
    args = gen_val_list(entity_chain_callback=gen_entity_chain_pair,
                        mask_token_args_only=mask_token_args_only) if is_func_call else ''

    # Generate generics
    generics = gen_provided_generics() if is_func_call else ''

    obj = f'{MASK_TOKEN}{generics}{func_call_markers[0]}{args}{func_call_markers[1]}'

    return obj


def gen_entity_chain_pair(add_semicolon=True, mask_token_args_only=False, should_add_mask_indices=True):
    """Generate an entity chain pair."""

    entities = []
    length_range = range(1, 11)
    length = random.choices(length_range, weights=(80, 70, 60, 40, 30, 20, 5, 4, 3, 2), k=1)[0]

    # Generate entities
    for i in range(length):
        entities.append(__gen_entity(mask_token_args_only=mask_token_args_only))

    source_entities = join_rand(entities, CPP_CHAIN_OPS)

    if should_add_mask_indices:
        source_entities, _ = add_mask_indices(source_entities)

    target_entities = join(entities, '.')

    if should_add_mask_indices:
        target_entities, _ = add_mask_indices(target_entities)

    semicolon = ';' if add_semicolon else ''

    source = f'{source_entities}{semicolon}'
    target = f'{target_entities}{semicolon}'
    return source, target


def gen_entity_chains(count):
    """Generate entity chains."""

    data = []

    for _ in tqdm(range(count), desc='Generating entity chains'):
        (source, target) = gen_entity_chain_pair()
        data.append(gen_item(source, target))

    return data
