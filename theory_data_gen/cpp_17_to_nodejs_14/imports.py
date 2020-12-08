from theory_data_gen.common import gen_item, gen_mask_token


def gen_path_imports():
    """Generate path imports."""

    m = gen_mask_token(0)
    return [
        gen_item(
            f'#include "{m}"',
            f'require("{m}");'
        ),
        gen_item(
            f'#include <{m}>',
            f'require("{m}");'
        )
    ]