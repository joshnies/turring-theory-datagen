from theory_data_gen.common import gen_item, gen_mask_token
from theory_data_gen.utils import join


def __gen_cout_pair(use_std, num_vals):
    """Generate console output pair."""

    std = 'std::' if use_std else ''
    source_vals = []
    target_vals = []

    for i in range(num_vals):
        m = gen_mask_token(i)
        source_vals.append(f'<< {m}')
        target_vals.append(f'${{{m}}}')

    source = f'{std}cout {join(source_vals, " ")} << {std}endl;'
    target = f'console.log(`{join(target_vals, "")}`);'
    return source, target


def gen_couts():
    """Generate console output statements."""

    data = []

    # Generate with "std::"
    for num_vals in range(1, 11):
        (source, target) = __gen_cout_pair(use_std=False, num_vals=num_vals)
        data.append(gen_item(source, target))

    # Generate without "std::"
    for num_vals in range(1, 11):
        (source, target) = __gen_cout_pair(use_std=True, num_vals=num_vals)
        data.append(gen_item(source, target))

    return data
