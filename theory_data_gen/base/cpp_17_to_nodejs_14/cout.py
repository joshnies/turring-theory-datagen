from theory_data_gen.common import gen_item, gen_mask_token
from theory_data_gen.utils import join


def __gen_cout_item(use_std, num_vals):
    """Generate console output pair."""

    std = 'std::' if use_std else ''
    source_vals = []
    target_vals = []

    for i in range(num_vals):
        m = gen_mask_token(i)
        source_vals.append(f'<< {m}')
        target_vals.append(f'${{{m}}}')

    return gen_item(
        f'{std}cout {join(source_vals, " ")} << {std}endl;',
        f'console.log(`{join(target_vals, "")}`);'
    )


def gen_couts(write):
    """Generate console output statements."""

    for num_vals in range(1, 11):
        items = [
            __gen_cout_item(use_std=False, num_vals=num_vals),
            __gen_cout_item(use_std=True, num_vals=num_vals)
        ]

        for i in items:
            write(i)
