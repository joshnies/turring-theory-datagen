from theory_data_gen.common import gen_mask_token, gen_item, add_open_bracket


def __gen_switch_structs():
    """Generate "switch" structure."""

    item_wo_open_bracket = gen_item(f'switch ({gen_mask_token(0)})')
    item_w_open_bracket = add_open_bracket(item_wo_open_bracket)

    return [
        item_wo_open_bracket,
        item_w_open_bracket
    ]


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
    items = __gen_switch_structs()
    data.extend(items)

    # Generate switch case statement
    switch_case = __gen_case()
    item = {'source': switch_case, 'target': switch_case}
    data.append(item)

    # Generate default switch case statement
    def_switch_case = __gen_default_case()
    item = {'source': def_switch_case, 'target': def_switch_case}
    data.append(item)

    return data
