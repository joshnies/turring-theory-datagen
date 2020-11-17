from tqdm import tqdm
from common import gen_item
from constants import AI_EXTRACTION, AI_VAR_NAME, AI_USER_TYPE
from cpp import CPP_PRIM_TYPES


def __gen_try_catch_block_pair(t):
    """Generate try-catch block pair."""

    source = f'try {{{AI_EXTRACTION}}} catch ({t} {AI_VAR_NAME}) {{{AI_EXTRACTION}}}'
    target = f'try {{{AI_EXTRACTION}}} catch ({AI_VAR_NAME}) {{{AI_EXTRACTION}}}'
    return source, target


def gen_try_catch_blocks():
    """Generate all try-catch block data."""

    types = CPP_PRIM_TYPES.copy()
    types.append(AI_USER_TYPE)
    data = []

    for t in tqdm(types, desc='Generating try-catch blocks'):
        (source, target) = __gen_try_catch_block_pair(t)
        data.append(gen_item(source, target))

    return data
