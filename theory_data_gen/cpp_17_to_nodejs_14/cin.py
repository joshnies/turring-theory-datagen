from theory_data_gen.common import gen_item, gen_mask_token


def __gen_cin_pair(use_std, num_vals):
    """Generate console input pair."""

    std = 'std::' if use_std else ''
    source_vals = []
    target_vals = []

    for i in range(num_vals):
        m = gen_mask_token(i)
        source_vals.append(f'>> {m}')
        target_vals.append(m)

    source = f'{std}cin {" ".join(source_vals)};'

    # Target uses "native-console" NPM package: https://www.npmjs.com/package/native-console
    target = f'[{", ".join(target_vals)}] = nativeConsole.read();'

    return source, target


def gen_cins():
    """Generate console input statements."""

    data = []

    # Generate with "std::"
    for num_vals in range(1, 11):
        (source, target) = __gen_cin_pair(use_std=False, num_vals=num_vals)
        data.append(gen_item(source, target))

    # Generate without "std::"
    for num_vals in range(1, 11):
        (source, target) = __gen_cin_pair(use_std=True, num_vals=num_vals)
        data.append(gen_item(source, target))

    return data
