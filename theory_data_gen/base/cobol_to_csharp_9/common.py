from typing import List, Tuple

from common import gen_mask_token


def gen_filler_pairs(pairs: List[Tuple[str, str]]) -> List[Tuple[str, str]]:
    """
    Generate "FILLER" variable pairs.

    :param pairs: Original pairs.
    :returns: Pairs with appended FILLER versions.
    """

    result = pairs.copy()
    m_name = gen_mask_token(1)

    filler_pairs = list(
        map(
            lambda p: (
                p[0].replace(m_name, 'FILLER')
                    .replace(gen_mask_token(2), gen_mask_token(1))
                    .replace(gen_mask_token(3), gen_mask_token(2))
                    .replace(gen_mask_token(4), gen_mask_token(3)),
                p[1].replace(m_name, '%filler_n%')
                    .replace(gen_mask_token(2), gen_mask_token(1))
                    .replace(gen_mask_token(3), gen_mask_token(2))
                    .replace(gen_mask_token(4), gen_mask_token(3))
            ),
            pairs
        )
    )

    result.extend(filler_pairs)
    return result
