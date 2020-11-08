from common import gen_item
from constants import AI_EXTRACTION


def gen_try_catch_blocks():
    """Generate try-catch blocks."""

    return [gen_item(f'try {{{AI_EXTRACTION}}} catch {{{AI_EXTRACTION}}}')]
