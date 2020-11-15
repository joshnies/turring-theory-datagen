import random

from common import gen_val_list
from constants import AI_STMT_OBJ
from generics import gen_provided_generics
from utils import join, join_rand


def gen_func_call_obj():
    """Generates an object or function call for a pair."""

    is_func_call = bool(random.getrandbits(1))
    func_call_markers = ['(', ')'] if is_func_call else ['', '']
    args = gen_val_list() if is_func_call else ''

    # Generate generics
    generics = gen_provided_generics() if is_func_call else ''

    obj = f'{AI_STMT_OBJ}{generics}{func_call_markers[0]}{args}{func_call_markers[1]}'
    return obj


def gen_func_call_pair():
    """Generate a function call pair."""

    length_range = range(1, 11)
    length = random.choices(length_range, weights=(80, 70, 60, 40, 30, 20, 5, 4, 3, 2), k=1)[0]
    objs = []

    # Generate func args
    for i in range(length):
        objs.append(gen_func_call_obj())

    source_objs = join_rand(objs, ['.', '->', '::'])
    target_objs = join(objs, '.')

    source = f'{source_objs};'
    target = f'{target_objs};'
    return source, target


def gen_func_calls(count):
    """Generate all function call data."""

    data = []

    for i in range(count):
        (source, target) = gen_func_call_pair()
        item = {'source': source, 'target': target}

        if item not in data:
            data.append(item)

    return data
