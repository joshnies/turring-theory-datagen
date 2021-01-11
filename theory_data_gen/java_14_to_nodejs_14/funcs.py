import random

from tqdm import tqdm

from theory_data_gen.common import gen_mask_token, gen_item, add_scope_open_token, add_line_end_token
from theory_data_gen.common.java import JAVA_PRIM_TYPES_W_MASK
from .java import gen_modifier_permutations


def __gen_func_arg_pair(mask_index=1):
    """Generate a function argument pair."""

    # Generate mask tokens
    m_arg_name = gen_mask_token(mask_index)
    m_def_val = gen_mask_token(mask_index + 1)

    # Generate arg pair
    source_return_type = random.choice(JAVA_PRIM_TYPES_W_MASK)
    default_val = m_def_val if bool(random.getrandbits(1)) else ''
    default_val_assign = ' = ' if default_val != '' else ''

    if default_val == '':
        last_mask_index = mask_index
    else:
        last_mask_index = mask_index + 1

    source_arg = f'{source_return_type} {m_arg_name}{default_val_assign}{default_val}'
    target_arg = f'{m_arg_name}{default_val_assign}{default_val}'
    return (source_arg, target_arg), last_mask_index


def __gen_func_items():
    """Generate function items."""

    source_return_type = random.choice(JAVA_PRIM_TYPES_W_MASK)
    arg_range = range(0, 11)
    arg_count = random.choices(arg_range, weights=(80, 70, 60, 40, 30, 20, 5, 4, 3, 2, 1), k=1)[0]
    args = list()

    # Generate func args
    next_arg_mask_index = 1

    for _ in range(arg_count):
        arg_pair, last_mask_index = __gen_func_arg_pair(next_arg_mask_index)
        next_arg_mask_index = last_mask_index + 1
        args.append(arg_pair)

    source_args = list()
    target_args = list()

    for a in args:
        source_args.append(a[0])
        target_args.append(a[1])

    source_args_str = ', '.join(source_args)
    target_args_str = ', '.join(target_args)

    # Generate mask tokens
    m_func_name = gen_mask_token(0)

    def gen_func_permutations(constructor=False):
        conditional_source_return_type = '' if constructor else source_return_type

        # Without ending "{"
        items = gen_modifier_permutations(
            gen_item(f'{conditional_source_return_type} {m_func_name}({source_args_str})'.strip(),
                     f'const {m_func_name} = ({target_args_str}) =>'))

        # With ending "{"
        items.extend(
            list(map(lambda i: add_scope_open_token(i), items))
        )

        # With line end tokens
        items.extend(
            list(map(lambda i: add_line_end_token(i, tar_token=' {}'), items))
        )

        # With open arg list (e.g. "void main (")
        items.extend(
            gen_modifier_permutations(gen_item(
                f'{conditional_source_return_type} {m_func_name}('.strip(),
                f'const {m_func_name} = ('
            ))
        )

        return items

    items = gen_func_permutations(constructor=False)
    items.extend(gen_func_permutations(constructor=True))

    return items


def gen_funcs(write, count):
    """Generate functions."""

    for _ in tqdm(range(count), desc='Generating functions'):
        for i in __gen_func_items():
            write(i)
