from yaspin import yaspin

from theory_data_gen.common import gen_item, gen_mask_token


def gen_core(write):
    """
    Generate core items.

    :param write: CSV output write function.
    """

    with yaspin(text='Generating core (common) statements...', color='magenta'):
        # Generate items
        pairs = [
            (
                f'ACCEPT {gen_mask_token(0)}.',
                f'{gen_mask_token(0)}.Set(Console.Read());'
            )
        ]

        items = list(map(lambda p: gen_item(p[0], p[1]), pairs))

        # Write items
        for i in items:
            write(i)
