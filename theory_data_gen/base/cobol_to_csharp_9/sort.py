from ...common import gen_item, gen_mask_token, add_mask_indices
from ...constants import MASK_TOKEN


def __gen_sort_items():
    items = list()

    # Generate items
    for n in range(1, 11):
        for i_is_asc in range(2):
            for i_use_prehook in range(2):
                for i_use_posthook in range(2):
                    src_keys = ' '.join([MASK_TOKEN] * n)
                    tar_keys = ', '.join([MASK_TOKEN] * n)

                    is_asc = bool(i_is_asc)
                    src_dir = 'ASCENDING' if is_asc else 'DESCENDING'
                    tar_dir = 'Ascending' if is_asc else 'Descending'

                    use_prehoook = bool(i_use_prehook)
                    use_posthook = bool(i_use_posthook)
                    prehook_mask_token = gen_mask_token(n + 1)
                    posthook_mask_token = gen_mask_token(n + 2) if use_prehoook else prehook_mask_token

                    src_prehook = f' INPUT PROCEDURE IS {prehook_mask_token}' if use_prehoook else ''
                    tar_prehook = fr'{prehook_mask_token}();\n' if use_prehoook else ''

                    src_posthook = f' OUTPUT PROCEDURE IS {posthook_mask_token}' if use_posthook else ''
                    tar_posthook = fr'\n{posthook_mask_token}();' if use_posthook else ''

                    src, _ = add_mask_indices(
                        f'SORT {MASK_TOKEN} ON {src_dir} KEY {src_keys}{src_prehook}{src_posthook}'
                    )
                    tar, _ = add_mask_indices(
                        f'{tar_prehook}{MASK_TOKEN}.Sort(direction: SortDirection.{tar_dir}, {tar_keys});{tar_posthook}'
                    )

                    items.append(gen_item(src, tar))

    return items


def gen_sort_statements(write):
    """
    Generate sort statements.

    :param write: CSV output write function.
    """

    for i in __gen_sort_items():
        write(i)
