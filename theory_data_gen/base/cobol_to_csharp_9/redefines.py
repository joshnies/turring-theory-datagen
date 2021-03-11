from base.cobol_to_csharp_9.common import gen_filler_pairs
from common import gen_mask_token, gen_item

# Generate static mask tokens
m_level = gen_mask_token(0)
m_name = gen_mask_token(1)
m_from = gen_mask_token(2)
m_size = gen_mask_token(3)


def __gen_str_redefines():
    pairs = [
        (
            f'{m_level} {m_name} REDEFINES {m_from} PIC X({m_size})',
            f'{m_name} = new COBOLVar({m_from}.ToString(), {m_size});'
        ),
    ]

    for src_type in ['X', 'A']:
        for i in range(1, 21):
            pairs.append((
                f"{m_level} {m_name} REDEFINES {m_from} PIC {src_type * i}",
                f'{m_name} = new COBOLVar({m_from}.ToString(), {i});'
            ))

    return pairs


def __gen_int_redefines():
    pairs = list()

    for is_signed in range(2):
        prefix = 'S' if is_signed else ''
        src_type = f'{prefix}9'

        pairs.append((
            f'{m_level} {m_name} REDEFINES {m_from} PIC {src_type}({m_size})',
            f'{m_name} = new COBOLVar(int.Parse({m_from}.value), {m_size});'
        ))

        for i in range(1, 11):
            type_range = '9' * i

            pairs.append((
                f"{m_level} {m_name} REDEFINES {m_from} PIC {prefix + type_range}",
                f'{m_name} = new COBOLVar(int.Parse({m_from}.value), {i});'
            ))

    return pairs


def __gen_float_redefines():
    pairs = list()

    for is_signed in range(2):
        prefix = 'S' if is_signed else ''
        src_type = f'{prefix}9'

        pairs.append((
            f'{m_level} {m_name} REDEFINES {m_from} PIC {src_type}({m_size})V9({gen_mask_token(4)})',
            f'{m_name} = new COBOLVar(float.Parse({m_from}.value), {m_size} + {gen_mask_token(4)} + 1);'
        ), )

        for i in range(1, 11):
            whole_type_range = '9' * i

            for k in range(1, 11):
                dec_type_range = '9' * i

                pairs.append((
                    f"{m_level} {m_name} REDEFINES {m_from} PIC {prefix + whole_type_range}V{dec_type_range}",
                    f'{m_name} = new COBOLVar(float.Parse({m_from}.value), {i + k});'
                ))

    return pairs


def gen_redefines(write):
    """
    Generate redefined variables.

    :param write: CSV output write function.
    """

    pairs = list()
    pairs.extend(__gen_str_redefines())
    pairs.extend(__gen_int_redefines())
    pairs.extend(__gen_float_redefines())

    # Add "FILLER" syntax pairs
    pairs = gen_filler_pairs(pairs)

    # Convert pairs to writable items
    items = map(lambda p: gen_item(p[0], p[1]), pairs)

    for i in items:
        write(i)
