from constants import AI_IMPORT


def gen_path_imports():
    """Generate path imports."""

    return [
        {
            'source': f'#include "{AI_IMPORT}"',
            'target': f'require("{AI_IMPORT}")'
        },
        {
            'source': f'#include <{AI_IMPORT}>',
            'target': f'require("{AI_IMPORT}")'
        }
    ]
