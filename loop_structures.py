from tqdm import tqdm
from constants import AI_EXTRACTION, AI_CONDITION, AI_FOR_LOOP_EXTRACTION, AI_VAR_NAME, AI_USER_TYPE
from cpp import CPP_PRIM_TYPES


def gen_for_pair():
    """Generate "for" loop structure pair."""

    return f'for ({AI_FOR_LOOP_EXTRACTION}) {{{AI_EXTRACTION}}}'


def gen_foreach_pair(t):
    """Generate "foreach" loop structure pair."""

    source = f'for ({t} {AI_VAR_NAME} : {AI_VAR_NAME}) {{{AI_EXTRACTION}}}'
    target = f'for ({t} {AI_VAR_NAME} of {AI_VAR_NAME}) {{{AI_EXTRACTION}}}'
    return source, target


def gen_while_pair():
    """Generate "while" loop structure pair."""

    return f'while ({AI_CONDITION}) {{{AI_EXTRACTION}}}'


def gen_do_while_pair():
    """Generate "do-while" loop structure pair."""

    return f'do {{{AI_EXTRACTION}}} while ({AI_CONDITION});'


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
    (source, target) = gen_foreach_pair(AI_USER_TYPE)
    item = {'source': source, 'target': target}
    data.append(item)

    # Generate "while" loop structure
    while_loop = gen_while_pair()
    item = {'source': while_loop, 'target': while_loop}
    data.append(item)

    # Generate "do-while" loop structure
    do_while_loop = gen_do_while_pair()
    item = {'source': do_while_loop, 'target': do_while_loop}
    data.append(item)

    return data
