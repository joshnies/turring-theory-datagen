import random

from theory_data_gen.constants import MASK_TOKEN
from .java import gen_java_generic_type


def gen_inheritance(count):
    """Generate a Java class inheritance sequence."""

    mask_tokens = list()

    for _ in range(count):
        new_token = gen_java_generic_type() if bool(random.getrandbits(1)) else MASK_TOKEN
        mask_tokens.append(new_token)

    extended_classes = ', '.join(mask_tokens)

    return f' extends {extended_classes}'


def gen_interface_implementations(count):
    """Generate a Java interface implementation sequence."""

    mask_tokens = list()

    for _ in range(count):
        new_token = gen_java_generic_type() if bool(random.getrandbits(1)) else MASK_TOKEN
        mask_tokens.append(new_token)

    implemented_interfaces = ', '.join(mask_tokens)

    return f' implements {implemented_interfaces}'
