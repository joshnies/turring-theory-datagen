from common import gen_item
from constants import AI_COPY


def gen_comments():
    """Generate all comment data."""

    data = [
        gen_item(f'//{AI_COPY}'),
        gen_item(f'/*{AI_COPY}*/')
    ]

    return data
