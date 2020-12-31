from tqdm import tqdm

from theory_data_gen.common import gen_item, add_mask_indices
from theory_data_gen.constants import MASK_TOKEN
from .lvp import TYPE_MAP


def __gen_catch_struct_item(src_type: str, tar_type: str):
    """Generate "catch" structure item."""

    src, _ = add_mask_indices(f'catch ({src_type} {MASK_TOKEN}) {{')
    tar, _ = add_mask_indices(f'except {tar_type}:')

    return gen_item(src, tar)


def gen_catch_blocks(write):
    """Generate all try-catch block data."""

    # Generate catch structures
    for k, v in tqdm(TYPE_MAP.items(), desc='Generating "catch" structures'):
        write(__gen_catch_struct_item(k, v))
