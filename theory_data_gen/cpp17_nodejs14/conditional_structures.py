def gen_if_pair():
    """Generate "if" structure pair."""

    # TODO: Add random boolean expression as condition
    if_struct = f'if () {{'
    return if_struct


def gen_else_if_pair():
    """Generate "else if" structure pair."""

    # TODO: Add random boolean expression as condition
    else_if_struct = f'else if () {{'
    return else_if_struct


def gen_else_pair():
    """Generate "else" structure pair."""

    else_if_struct = f'else {{'
    return else_if_struct


def gen_conditional_structs():
    """Generate all conditional structure data."""

    data = []

    # Generate "if" structure
    if_struct = gen_if_pair()
    item = {'source': if_struct, 'target': if_struct}
    data.append(item)

    # Generate "else if" structure
    else_if_struct = gen_else_if_pair()
    item = {'source': else_if_struct, 'target': else_if_struct}
    data.append(item)

    # Generate "else" structure
    else_struct = gen_else_pair()
    item = {'source': else_struct, 'target': else_struct}
    data.append(item)

    return data
