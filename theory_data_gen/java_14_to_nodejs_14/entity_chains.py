import random
from tqdm import tqdm

from theory_data_gen.common import add_mask_indices, gen_item
from theory_data_gen.utils import join
from theory_data_gen.constants import MASK_TOKEN
from .generics import gen_provided_generics
from .val_lists import gen_val_list


def __gen_entity(mask_token_args_only=False):
    """Generates an entity (object or function call) for an entity chain pair."""

    # Generate data
    is_func_call = bool(random.getrandbits(1))
    func_call_markers = ['(', ')'] if is_func_call else ['', '']
    source_args, target_args = gen_val_list(entity_chain_callback=gen_entity_chain_pair,
                                            mask_token_args_only=mask_token_args_only) if is_func_call else ('', '')

    # Generate generics
    generics = gen_provided_generics() if is_func_call else ''

    # Generate final output
    source = f'{MASK_TOKEN}{generics}{func_call_markers[0]}{source_args}{func_call_markers[1]}'
    target = f'{MASK_TOKEN}{generics}{func_call_markers[0]}{target_args}{func_call_markers[1]}'

    return source, target


def gen_entity_chain_pair(add_semicolon=True, mask_token_args_only=False, should_add_mask_indices=True):
    """Generate an entity chain pair."""

    source_entities = []
    target_entities = []
    length_range = range(1, 11)
    length = random.choices(length_range, weights=(80, 70, 60, 40, 30, 20, 5, 4, 3, 2), k=1)[0]

    # Generate entities
    for i in range(length):
        s, t = __gen_entity(mask_token_args_only=mask_token_args_only)
        source_entities.append(s)
        target_entities.append(t)

    source = join(source_entities, '.')
    target = join(target_entities, '.')

    # Add mask indices
    if should_add_mask_indices:
        source, _ = add_mask_indices(source)
        target, _ = add_mask_indices(target)

    semicolon = ';' if add_semicolon else ''

    source = f'{source}{semicolon}'
    target = f'{target}{semicolon}'
    return source, target


def gen_entity_chains(write, count):
    """Generate entity chains."""

    for _ in tqdm(range(count), desc='Generating entity chains'):
        src, tar = gen_entity_chain_pair()
        item = gen_item(src, tar)
        write(item)
