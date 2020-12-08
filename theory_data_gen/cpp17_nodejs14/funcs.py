import random

from tqdm import tqdm

from common import gen_mask_token
from theory_data_gen.cpp17_nodejs14.cpp import CPP_PRIM_TYPES
from theory_data_gen.utils import join


def gen_func_arg_pair(mask_index=1):
    """Generate a function argument pair."""

    # Generate mask tokens
    m_arg_name = gen_mask_token(mask_index)
    m_def_val = gen_mask_token(mask_index + 1)

    # Generate arg pair
    source_return_type = random.choice(CPP_PRIM_TYPES)
    pointer = '*' if bool(random.getrandbits(1)) else ''
    default_val = m_def_val if bool(random.getrandbits(1)) else ''
    default_val_assign = ' = ' if default_val != '' else ''

    if default_val == '':
        last_mask_index = mask_index
    else:
        last_mask_index = mask_index + 1

    source_arg = f'{source_return_type}{pointer} {m_arg_name}{default_val_assign}{default_val}'
    target_arg = f'{m_arg_name}{default_val_assign}{default_val}'
    return (source_arg, target_arg), last_mask_index


def gen_func_pair():
    """Generate a function pair."""

    abstract = 'abstract ' if bool(random.getrandbits(1)) else ''
    source_return_type = random.choice(CPP_PRIM_TYPES)
    pointer = '*' if bool(random.getrandbits(1)) else ''
    arg_range = range(0, 11)
    arg_count = random.choices(arg_range, weights=(80, 70, 60, 40, 30, 20, 5, 4, 3, 2, 1), k=1)[0]
    args = []

    # Generate func args
    next_arg_mask_index = 1

    for _ in range(arg_count):
        arg_pair, last_mask_index = gen_func_arg_pair(next_arg_mask_index)
        next_arg_mask_index = last_mask_index + 1
        args.append(arg_pair)

    source_args = []
    target_args = []

    for a in args:
        source_args.append(a[0])
        target_args.append(a[1])

    source_args_str = join(source_args, ', ')
    target_args_str = join(target_args, ', ')

    # Generate mask tokens
    m_func_name = gen_mask_token(0)

    # Generate final functions
    source_func = f'{abstract}{source_return_type}{pointer} {m_func_name}({source_args_str}) {{'
    target_func = f'const {m_func_name} = ({target_args_str}) => {{'
    return source_func, target_func


def gen_funcs(count):
    """Generate all function data."""

    data = []

    for _ in tqdm(range(count), desc='Generating functions'):
        (source, target) = gen_func_pair()
        item = {'source': source, 'target': target}
        data.append(item)

    return data
