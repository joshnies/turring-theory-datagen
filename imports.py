from common import gen_item
from constants import AI_IMPORT


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
