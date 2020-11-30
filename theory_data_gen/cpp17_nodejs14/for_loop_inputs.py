from tqdm import tqdm
from theory_data_gen.mask_tokens import AI_USER_TYPE, AI_VAL, AI_CONDITION, AI_EXTRACTION
from theory_data_gen.cpp17_nodejs14.cpp import CPP_PRIM_TYPES


def gen_for_loop_input_pair(t):
    """Generate "for" loop input pair."""

    source = f'{t} {AI_USER_TYPE} = {AI_VAL}; {AI_CONDITION}; {AI_EXTRACTION}'
    target = f'let {AI_USER_TYPE} = {AI_VAL}; {AI_CONDITION}; {AI_EXTRACTION}'
    return source, target


def gen_for_loop_inputs():
    """Generate all "for" loop inputs."""

    data = []

    # Generate "for" loop input pairs for pre-defined types
    for t in tqdm(CPP_PRIM_TYPES, desc='Generating "for" loop inputs'):
        (source, target) = gen_for_loop_input_pair(t)
        item = {'source': source, 'target': target}
        data.append(item)

    # Generate "for" loop input pair for user type
    (source, target) = gen_for_loop_input_pair(AI_USER_TYPE)
    item = {'source': source, 'target': target}
    data.append(item)

    return data
