import random

from tqdm import tqdm

from theory_data_gen.common import gen_mask_token, gen_item, add_open_bracket
from theory_data_gen.utils import join
from .cpp import CPP_PRIM_TYPES, gen_mem_symbol


def __gen_func_arg_pair(mask_index=1):
    """Generate a function argument pair."""

    # Generate mask tokens
    m_arg_name = gen_mask_token(mask_index)
    m_def_val = gen_mask_token(mask_index + 1)

    # Generate arg pair
    const = 'const ' if random.choice(range(10)) == 0 else ''
    source_return_type = f'{const}{random.choice(CPP_PRIM_TYPES)}'
    default_val = m_def_val if bool(random.getrandbits(1)) else ''
    default_val_assign = ' = ' if default_val != '' else ''
    cpp_array_symbols = '[]' if random.choice(range(10)) == 0 else ''
    mem_symbol = gen_mem_symbol()

    if default_val == '':
        last_mask_index = mask_index
    else:
        last_mask_index = mask_index + 1

    source_arg = f'{source_return_type}{mem_symbol} {m_arg_name}{cpp_array_symbols}{default_val_assign}{default_val}'
    target_arg = f'{m_arg_name}{default_val_assign}{default_val}'
    return (source_arg, target_arg), last_mask_index


def __gen_func_items():
    """Generate function items."""

    abstract = 'abstract ' if bool(random.getrandbits(1)) else ''
    source_return_type = random.choice(CPP_PRIM_TYPES)
    pointer = '*' if bool(random.getrandbits(1)) else ''
    arg_range = range(0, 11)
    arg_count = random.choices(arg_range, weights=(80, 70, 60, 40, 30, 20, 5, 4, 3, 2, 1), k=1)[0]
    args = []

    # Generate func args
    next_arg_mask_index = 1

    for _ in range(arg_count):
        arg_pair, last_mask_index = __gen_func_arg_pair(next_arg_mask_index)
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

    # Generate function signatures
    # Without ending "{"
    item_wo_open_bracket = gen_item(f'{abstract}{source_return_type}{pointer} {m_func_name}({source_args_str})',
                                    f'const {m_func_name} = ({target_args_str}) =>')

    # With ending "{"
    item_w_open_bracket = add_open_bracket(item_wo_open_bracket)

    # With open arg list (e.g. "void main (")
    item_open_args = gen_item(f'{abstract}{source_return_type}{pointer} {m_func_name}(', f'const {m_func_name} = (')

    return [
        item_wo_open_bracket,
        item_w_open_bracket,
        item_open_args
    ]


def gen_funcs(write, count):
    """Generate functions."""

    for _ in tqdm(range(count), desc='Generating functions'):
        for i in __gen_func_items():
            write(i)
