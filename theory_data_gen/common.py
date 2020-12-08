import random
import re
from typing import List, Tuple

from theory_data_gen.mask_tokens import AI_VAL, MASK_TOKEN
from theory_data_gen.utils import join


def gen_val_list():
    """Generate a value list."""

    val_range = range(0, 11)
    val_count = random.choices(val_range, weights=(80, 70, 60, 40, 30, 20, 5, 4, 3, 2, 1), k=1)[0]
    vals = []

    # Generate value list
    for i in range(val_count):
        vals.append(AI_VAL)

    return join(vals, ', ')


def gen_item(source, target=None):
    """Generate dataset item."""

    if target is None:
        target = source

    return {'source': source, 'target': target}


def deduplicate(data):
    """Deduplicate data."""

    return [dict(t) for t in {tuple(d.items()) for d in data}]


# def permutate(src_seq: str, tar_seq: str, permutations: int):
#     """Generate mask permutations of the given masked sequences."""
#
#     data = []
#
#     # Get amount of mask tokens
#     count = len(re.findall(MASK_TOKEN, src_seq))
#     pm_list = list(range(count))
#
#     for i in range(permutations):
#         pm_list = [x + i for x in pm_list]
#         pm_src_seq = src_seq
#         pm_tar_seq = tar_seq
#
#         for match_idx in range(len(re.findall(MASK_TOKEN, src_seq))):
#             pm_src_seq = pm_src_seq.replace(MASK_TOKEN, MASK_TOKEN.replace('n', str(pm_list[match_idx])))
#             pm_tar_seq = pm_tar_seq.replace(MASK_TOKEN, MASK_TOKEN.replace('n', str(pm_list[match_idx])))
#
#         data.append(gen_item(pm_src_seq, pm_tar_seq))
#
#     return data


def gen_mask_token(i):
    """Generate mask token with index."""

    return MASK_TOKEN.replace('n', str(i), 1)
