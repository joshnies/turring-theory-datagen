from theory_data_gen.common import gen_mask_token


def __gen_switch_struct():
    """Generate "switch" structure."""

    return f'switch ({gen_mask_token(0)}) {{'


def __gen_case():
    """Generate case statement."""

    return f'case {gen_mask_token(0)}:'


def __gen_default_case():
    """Generate "default" case statement."""

    return 'default:'


def gen_switch_data():
    """Generate "switch" structures and case statements."""

    data = []

    # Generate "switch" structure
    switch_struct = __gen_switch_struct()
    item = {'source': switch_struct, 'target': switch_struct}
    data.append(item)

    # Generate switch case statement
    switch_case = __gen_case()
    item = {'source': switch_case, 'target': switch_case}
    data.append(item)

    # Generate default switch case statement
    def_switch_case = __gen_default_case()
    item = {'source': def_switch_case, 'target': def_switch_case}
    data.append(item)

    return data
