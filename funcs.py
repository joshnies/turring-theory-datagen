import random

from constants import AI_VAL, AI_EXTRACTION, AI_ARG_NAME, AI_FUNC_NAME
from cpp import CPP_PRIM_TYPES
from utils import join


def gen_func_arg_pair():
    """Generate a function argument pair."""

    source_return_type = random.choice(CPP_PRIM_TYPES)
    pointer = '*' if bool(random.getrandbits(1)) else ''
    default_val = AI_VAL if bool(random.getrandbits(1)) else ''
    default_val_assign = ' = ' if default_val != '' else ''

    source_arg = f'{source_return_type}{pointer} {AI_ARG_NAME}{default_val_assign}{default_val}'
    target_arg = AI_ARG_NAME
    return source_arg, target_arg


def gen_func_pair():
    """Generate a function pair."""

    source_return_type = random.choice(CPP_PRIM_TYPES)
    pointer = '*' if bool(random.getrandbits(1)) else ''
    body = AI_EXTRACTION if bool(random.getrandbits(1)) else ''
    arg_range = range(0, 11)
    arg_count = random.choices(arg_range, weights=(80, 70, 60, 40, 30, 20, 5, 4, 3, 2, 1), k=1)[0]
    args = []

    # Generate func args
    for _ in range(arg_count):
        args.append(gen_func_arg_pair())

    source_args = []
    target_args = []

    for a in args:
        source_args.append(a[0])
        target_args.append(a[1])

    source_args_str = join(source_args, ', ')
    target_args_str = join(target_args, ', ')

    # Generate final functions
    source_func = f'{source_return_type}{pointer} {AI_FUNC_NAME}({source_args_str}) {{{body}}}'
    target_func = f'const {AI_FUNC_NAME} => ( {target_args_str} ) {{ {body} }}'
    return source_func, target_func


def gen_funcs(count):
    """Generate all function data."""

    data = []

    for _ in range(count):
        (source, target) = gen_func_pair()
        item = {'source': source, 'target': target}

        if item not in data:
            data.append(item)

    return data
