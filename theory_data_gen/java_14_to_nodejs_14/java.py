from theory_data_gen.common import gen_item
from theory_data_gen.common.java import JAVA_ACCESS_MODIFIERS


def gen_modifier_permutations(item, include_abstract=True, include_static=True, include_final=True):
    """
    Generate permutations of an item with prefixed modifiers (such as access modifiers).

    :param item: Item.
    :param include_abstract: Whether to generate permutations with "abstract" modifiers.
    :param include_static: Whether to generate permutations with "static" modifiers.
    :param include_final: Whether to generate permutations with "final" modifiers.
    :returns: List of item permutations with modifiers.
    """

    src = item['source']
    tar = item['target']
    new_items = list()

    mods = JAVA_ACCESS_MODIFIERS.copy()
    mods.append('')

    for m in mods:
        new_items.append((f'{m} {src}', tar))

        if include_abstract:
            new_items.append((f'{m} abstract {src}', f'{tar}'))

            if include_static:
                new_items.append((f'{m} static abstract {src}', f'static {tar}'))
                new_items.append((f'{m} abstract static {src}', f'static {tar}'))

        if include_static:
            new_items.append((f'{m} static {src}', f'static {tar}'))

            if include_final:
                new_items.append((f'{m} static final {src}', f'static {tar}'))
                new_items.append((f'{m} final static {src}', f'static {tar}'))

        if include_final:
            new_items.append((f'{m} final {src}', f'{tar}'))

    new_items = list(map(lambda i: gen_item(i[0].strip(), i[1].strip()), new_items))

    return new_items
