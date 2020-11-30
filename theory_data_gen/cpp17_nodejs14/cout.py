from theory_data_gen.common import gen_item
from theory_data_gen.mask_tokens import AI_VAL
from theory_data_gen.utils import join


def __gen_cout_pair(use_std, num_vals):
    """Generate console output pair."""

    std = 'std::' if use_std else ''
    source_vals = []
    target_vals = []

    for _ in range(num_vals):
        source_vals.append(f'<< {AI_VAL}')
        target_vals.append(f'${{{AI_VAL}}}')

    source = f'{std}cout {join(source_vals, " ")} << {std}endl;'
    target = f'console.log(`{join(target_vals, "")}`);'
    return source, target


def gen_couts():
    """Generate all console output statement data."""

    data = []

    for num_vals in range(1, 11):
        (source, target) = __gen_cout_pair(use_std=False, num_vals=num_vals)
        data.append(gen_item(source, target))

    for num_vals in range(1, 11):
        (source, target) = __gen_cout_pair(use_std=True, num_vals=num_vals)
        data.append(gen_item(source, target))

    return data
