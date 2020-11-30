from theory_data_gen.common import gen_item
from theory_data_gen.mask_tokens import AI_COPY


def gen_comments():
    """Generate all comment data."""

    data = [
        gen_item(f'//{AI_COPY}'),
        gen_item(f'/*{AI_COPY}*/')
    ]

    return data
