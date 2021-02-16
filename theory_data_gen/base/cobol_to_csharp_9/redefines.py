from common import gen_mask_token, gen_item


def gen_redefines(write):
    """
    Generate redefined variables.

    :param write: CSV output write function.
    """

    # Generate static mask tokens
    m_level = gen_mask_token(0)
    m_name = gen_mask_token(1)
    m_from = gen_mask_token(2)
    m_size = gen_mask_token(3)

    pairs = [
        # Alphanumeric
        (
            f'{m_level} {m_name} REDEFINES {m_from} PIC X',
            f'var {m_name} = new COBOLVar({m_from}.ToString(), 1);'
        ),
        (
            f'{m_level} {m_name} REDEFINES {m_from} PIC XX',
            f'var {m_name} = new COBOLVar({m_from}.ToString(), 2);'
        ),
        (
            f'{m_level} {m_name} REDEFINES {m_from} PIC XXX',
            f'var {m_name} = new COBOLVar({m_from}.ToString(), 3);'
        ),
        (
            f'{m_level} {m_name} REDEFINES {m_from} PIC XXXX',
            f'var {m_name} = new COBOLVar({m_from}.ToString(), 4);'
        ),
        (
            f'{m_level} {m_name} REDEFINES {m_from} PIC X({m_size})',
            f'var {m_name} = new COBOLVar({m_from}.ToString(), {m_size});'
        ),
        # Alphabetic
        (
            f'{m_level} {m_name} REDEFINES {m_from} PIC A',
            f'var {m_name} = new COBOLVar({m_from}.ToString(), 1);'
        ),
        (
            f'{m_level} {m_name} REDEFINES {m_from} PIC AA',
            f'var {m_name} = new COBOLVar({m_from}.ToString(), 2);'
        ),
        (
            f'{m_level} {m_name} REDEFINES {m_from} PIC AAA',
            f'var {m_name} = new COBOLVar({m_from}.ToString(), 3);'
        ),
        (
            f'{m_level} {m_name} REDEFINES {m_from} PIC AAAA',
            f'var {m_name} = new COBOLVar({m_from}.ToString(), 4);'
        ),
        (
            f'{m_level} {m_name} REDEFINES {m_from} PIC A({m_size})',
            f'var {m_name} = new COBOLVar({m_from}.ToString(), {m_size});'
        ),
        # Integers
        (
            f'{m_level} {m_name} REDEFINES {m_from} PIC 9',
            f'var {m_name} = new COBOLVar(int.Parse({m_from}), 1);'
        ),
        (
            f'{m_level} {m_name} REDEFINES {m_from} PIC 99',
            f'var {m_name} = new COBOLVar(int.Parse({m_from}), 2);'
        ),
        (
            f'{m_level} {m_name} REDEFINES {m_from} PIC 999',
            f'var {m_name} = new COBOLVar(int.Parse({m_from}), 3);'
        ),
        (
            f'{m_level} {m_name} REDEFINES {m_from} PIC 9999',
            f'var {m_name} = new COBOLVar(int.Parse({m_from}), 4);'
        ),
        (
            f'{m_level} {m_name} REDEFINES {m_from} PIC 9({m_size})',
            f'var {m_name} = new COBOLVar(int.Parse({m_from}), {m_size});'
        ),
        # Signed integers
        (
            f'{m_level} {m_name} REDEFINES {m_from} PIC S9',
            f'var {m_name} = new COBOLVar(int.Parse({m_from}), 1);'
        ),
        (
            f'{m_level} {m_name} REDEFINES {m_from} PIC S99',
            f'var {m_name} = new COBOLVar(int.Parse({m_from}), 2);'
        ),
        (
            f'{m_level} {m_name} REDEFINES {m_from} PIC S999',
            f'var {m_name} = new COBOLVar(int.Parse({m_from}), 3);'
        ),
        (
            f'{m_level} {m_name} REDEFINES {m_from} PIC S9999',
            f'var {m_name} = new COBOLVar(int.Parse({m_from}), 4);'
        ),
        (
            f'{m_level} {m_name} REDEFINES {m_from} PIC S9({m_size})',
            f'var {m_name} = new COBOLVar(int.Parse({m_from}), {m_size});'
        ),
        # Floats
        (
            f'{m_level} {m_name} REDEFINES {m_from} PIC 9V9',
            f'var {m_name} = new COBOLVar(float.Parse({m_from}), 3);'
        ),
        (
            f'{m_level} {m_name} REDEFINES {m_from} PIC 9V99',
            f'var {m_name} = new COBOLVar(float.Parse({m_from}), 4);'
        ),
        (
            f'{m_level} {m_name} REDEFINES {m_from} PIC 9V999',
            f'var {m_name} = new COBOLVar(float.Parse({m_from}), 5);'
        ),
        (
            f'{m_level} {m_name} REDEFINES {m_from} PIC 99V99',
            f'var {m_name} = new COBOLVar(float.Parse({m_from}), 5);'
        ),
        (
            f'{m_level} {m_name} REDEFINES {m_from} PIC 99V999',
            f'var {m_name} = new COBOLVar(float.Parse({m_from}), 6);'
        ),
        (
            f'{m_level} {m_name} REDEFINES {m_from} PIC 99V9999',
            f'var {m_name} = new COBOLVar(float.Parse({m_from}), 7);'
        ),
        (
            f'{m_level} {m_name} REDEFINES {m_from} PIC 9({m_size})V9({gen_mask_token(4)})',
            f'var {m_name} = new COBOLVar(float.Parse({m_from}), {m_size} + {gen_mask_token(4)} + 1);'
        ),
        # Signed floats
        (
            f'{m_level} {m_name} REDEFINES {m_from} PIC S9V9',
            f'var {m_name} = new COBOLVar(float.Parse({m_from}), 3);'
        ),
        (
            f'{m_level} {m_name} REDEFINES {m_from} PIC S9V99',
            f'var {m_name} = new COBOLVar(float.Parse({m_from}), 4);'
        ),
        (
            f'{m_level} {m_name} REDEFINES {m_from} PIC S9V999',
            f'var {m_name} = new COBOLVar(float.Parse({m_from}), 5);'
        ),
        (
            f'{m_level} {m_name} REDEFINES {m_from} PIC S99V99',
            f'var {m_name} = new COBOLVar(float.Parse({m_from}), 5);'
        ),
        (
            f'{m_level} {m_name} REDEFINES {m_from} PIC S99V999',
            f'var {m_name} = new COBOLVar(float.Parse({m_from}), 6);'
        ),
        (
            f'{m_level} {m_name} REDEFINES {m_from} PIC S99V9999',
            f'var {m_name} = new COBOLVar(float.Parse({m_from}), 7);'
        ),
        (
            f'{m_level} {m_name} REDEFINES {m_from} PIC S9({m_size})V9({gen_mask_token(4)})',
            f'var {m_name} = new COBOLVar(float.Parse({m_from}), {m_size} + {gen_mask_token(4)} + 1);'
        ),
    ]

    # Convert pairs to writable items
    items = map(lambda p: gen_item(p[0], p[1]), pairs)

    for i in items:
        write(i)
