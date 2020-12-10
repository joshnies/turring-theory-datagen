def __gen_if_struct():
    """Generate "if" structure."""

    # TODO: Add random boolean expression as condition
    return 'if () {{'


def __gen_else_if_struct():
    """Generate "else if" structure."""

    # TODO: Add random boolean expression as condition
    return 'else if () {{'


def __gen_else_struct():
    """Generate "else" structure."""

    return 'else {{'


def gen_conditional_structs():
    """Generate conditional structures."""

    data = []

    # Generate "if" structure
    if_struct = __gen_if_struct()
    item = {'source': if_struct, 'target': if_struct}
    data.append(item)

    # Generate "else if" structure
    else_if_struct = __gen_else_if_struct()
    item = {'source': else_if_struct, 'target': else_if_struct}
    data.append(item)

    # Generate "else" structure
    else_struct = __gen_else_struct()
    item = {'source': else_struct, 'target': else_struct}
    data.append(item)

    return data
