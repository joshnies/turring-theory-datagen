from theory_data_gen.constants import MASK_TOKEN


def gen_inheritance(count):
    """Generate a Java class inheritance sequence."""

    mask_tokens = []

    for _ in range(count):
        mask_tokens.append(MASK_TOKEN)

    extended_classes = ', '.join(mask_tokens)

    return f' extends {extended_classes}'


def gen_interface_implementations(count):
    """Generate a Java interface implementation sequence."""

    mask_tokens = []

    for _ in range(count):
        mask_tokens.append(MASK_TOKEN)

    implemented_interfaces = ', '.join(mask_tokens)

    return f' implements {implemented_interfaces}'