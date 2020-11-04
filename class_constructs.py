import random
from constants import AI_CLASS_NAME, AI_VAL
from utils import join


def gen_class_construct_pair():
    """Generate a class construction pair."""

    arg_range = range(0, 11)
    arg_count = random.choices(arg_range, weights=(80, 70, 60, 40, 30, 20, 5, 4, 3, 2, 1), k=1)[0]
    args = []

    # Generate args
    for i in range(arg_count):
        args.append(AI_VAL)

    args = join(args, ', ')

    construct = f'new {AI_CLASS_NAME}({args});'
    return construct


def gen_class_constructs(count):
    """Generate all class construction data."""

    data = []

    for i in range(count):
        construct = gen_class_construct_pair()
        item = {'source': construct, 'target': construct}
        data.append(item)

    return data
