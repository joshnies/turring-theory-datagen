from theory_data_gen.common import gen_item, gen_mask_token


def __gen_cin_item(use_std, num_vals):
    """
    Generate console input item.

    Target uses "native-console" NPM package: https://www.npmjs.com/package/native-console
    """

    std = 'std::' if use_std else ''
    source_vals = []
    target_vals = []

    for i in range(num_vals):
        m = gen_mask_token(i)
        source_vals.append(f'>> {m}')
        target_vals.append(m)

    return gen_item(
        f'{std}cin {" ".join(source_vals)};',
        f'[{", ".join(target_vals)}] = nativeConsole.read();'
    )


def gen_cins(write):
    """Generate console input statements."""

    for num_vals in range(1, 11):
        items = [
            __gen_cin_item(use_std=False, num_vals=num_vals),
            __gen_cin_item(use_std=True, num_vals=num_vals)
        ]

        for i in items:
            write(i)
