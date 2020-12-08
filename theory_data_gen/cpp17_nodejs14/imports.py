from theory_data_gen.common import gen_item
from theory_data_gen.mask_tokens import MASK_TOKEN


def gen_path_imports(permutations: int):
    """Generate path imports."""

    # return [
    #     gen_item(
    #         f'#include "{AI_IMPORT}"',
    #         f'require("{AI_IMPORT}")'
    #     ),
    #     gen_item(
    #         f'#include <{AI_IMPORT}>',
    #         f'require("{AI_IMPORT}")'
    #     )
    # ]

    data = []

    for i in range(permutations):
        m = MASK_TOKEN.replace('n', str(i), 1)
        items = [
            gen_item(
                f'#include "{m}"',
                f'require("{m}");'
            ),
            gen_item(
                f'#include <{m}>',
                f'require("{m}");'
            )
        ]

        data.extend(items)

    return data
