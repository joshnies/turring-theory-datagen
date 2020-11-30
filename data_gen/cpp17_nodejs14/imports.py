from data_gen.common import gen_item
from data_gen.mask_tokens import AI_IMPORT


def gen_path_imports():
    """Generate path imports."""

    return [
        gen_item(
            f'#include "{AI_IMPORT}"',
            f'require("{AI_IMPORT}")'
        ),
        gen_item(
            f'#include <{AI_IMPORT}>',
            f'require("{AI_IMPORT}")'
        )
    ]
