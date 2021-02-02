from theory_data_gen.common import gen_item


def gen_core():
    """Generate core items."""

    pairs = list()

    items = map(lambda p: gen_item(p), pairs)
    return items
