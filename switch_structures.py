from constants import AI_CONDITION, AI_EXTRACTION


def gen_switch_pair():
    """Generate "switch" structure pair."""

    return f'switch ({AI_CONDITION}) {{{AI_EXTRACTION}}}'


def gen_case_pair():
    """Generate "case" statement pair."""

    return f'case {AI_CONDITION}:'


def gen_default_case_pair():
    """Generate "default" case statement pair."""

    return 'default:'


def gen_switch_data():
    """Generate all "switch" structure data."""

    data = []

    # Generate "switch" structure
    switch_struct = gen_switch_pair()
    item = {'source': switch_struct, 'target': switch_struct}
    data.append(item)

    # Generate switch case statement
    switch_case = gen_case_pair()
    item = {'source': switch_case, 'target': switch_case}
    data.append(item)

    # Generate default switch case statement
    def_switch_case = gen_default_case_pair()
    item = {'source': def_switch_case, 'target': def_switch_case}
    data.append(item)

    return data
