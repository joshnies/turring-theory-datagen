from yaspin import yaspin

from ...common import gen_item, gen_mask_token


def gen_vars(write):
    """
    Generate variables.

    :param write: CSV output write function.
    """

    with yaspin(text='Generating variables...', color='magenta'):
        # Generate static mask tokens
        m_level = gen_mask_token(0)
        m_name = gen_mask_token(1)
        m_size = gen_mask_token(2)

        # Definitions without default value
        pairs = [
            # Alphanumeric
            (
                f'{m_level} {m_name} PIC X',
                f'var {m_name} = new COBOLVar("", 1);'
            ),
            (
                f'{m_level} {m_name} PIC XX',
                f'var {m_name} = new COBOLVar("", 2);'
            ),
            (
                f'{m_level} {m_name} PIC XXX',
                f'var {m_name} = new COBOLVar("", 3);'
            ),
            (
                f'{m_level} {m_name} PIC XXXX',
                f'var {m_name} = new COBOLVar("", 4);'
            ),
            (
                f'{m_level} {m_name} PIC X({m_size})',
                f'var {m_name} = new COBOLVar("", {m_size});'
            ),
            # Alphabetic
            (
                f'{m_level} {m_name} PIC A',
                f'var {m_name} = new COBOLVar("", 1);'
            ),
            (
                f'{m_level} {m_name} PIC AA',
                f'var {m_name} = new COBOLVar("", 2);'
            ),
            (
                f'{m_level} {m_name} PIC AAA',
                f'var {m_name} = new COBOLVar("", 3);'
            ),
            (
                f'{m_level} {m_name} PIC AAAA',
                f'var {m_name} = new COBOLVar("", 4);'
            ),
            (
                f'{m_level} {m_name} PIC A({m_size})',
                f'var {m_name} = new COBOLVar("", {m_size});'
            ),
            # Integers
            (
                f'{m_level} {m_name} PIC 9',
                f'var {m_name} = new COBOLVar(0, 1);'
            ),
            (
                f'{m_level} {m_name} PIC 99',
                f'var {m_name} = new COBOLVar(0, 2);'
            ),
            (
                f'{m_level} {m_name} PIC 999',
                f'var {m_name} = new COBOLVar(0, 3);'
            ),
            (
                f'{m_level} {m_name} PIC 9999',
                f'var {m_name} = new COBOLVar(0, 4);'
            ),
            (
                f'{m_level} {m_name} PIC 9({m_size})',
                f'var {m_name} = new COBOLVar(0, {m_size});'
            ),
            # Signed integers
            (
                f'{m_level} {m_name} PIC S9',
                f'var {m_name} = new COBOLVar(0, 1);'
            ),
            (
                f'{m_level} {m_name} PIC S99',
                f'var {m_name} = new COBOLVar(0, 2);'
            ),
            (
                f'{m_level} {m_name} PIC S999',
                f'var {m_name} = new COBOLVar(0, 3);'
            ),
            (
                f'{m_level} {m_name} PIC S9999',
                f'var {m_name} = new COBOLVar(0, 4);'
            ),
            (
                f'{m_level} {m_name} PIC S9({m_size})',
                f'var {m_name} = new COBOLVar(0, {m_size});'
            ),
            # Floats
            (
                f'{m_level} {m_name} PIC 9V9',
                f'var {m_name} = new COBOLVar(0f, 3);'
            ),
            (
                f'{m_level} {m_name} PIC 9({m_size})V9({gen_mask_token(3)})',
                # NOTE: Literal operations are calculated at compile-time in C#
                f'var {m_name} = new COBOLVar(0f, {m_size} + {gen_mask_token(3)} + 1);'
            ),
            # Signed floats
            (
                f'{m_level} {m_name} PIC S9V9',
                f'var {m_name} = new COBOLVar(0f, 3);'
            ),
            (
                f'{m_level} {m_name} PIC S9({m_size})V9({gen_mask_token(3)})',
                # NOTE: Literal operations are calculated at compile-time in C#
                f'var {m_name} = new COBOLVar(0f, {m_size} + {gen_mask_token(3)} + 1);'
            ),
        ]

        # Definitions with default value
        pairs_with_values = [
            (
                f'{m_level} {m_name} VALUE NULL',
                f'var {m_name} = new COBOLVar(null, 1);'
            ),
            (
                f'{m_level} {m_name} PIC X VALUE \' \'',
                f'var {m_name} = new COBOLVar("{gen_mask_token(2)}", 1);'
            ),
            (
                f'{m_level} {m_name} PIC X VALUE \'{gen_mask_token(2)}\'',
                f'var {m_name} = new COBOLVar("{gen_mask_token(2)}", 1);'
            ),
            (
                f'{m_level} {m_name} PIC XX VALUE \'{gen_mask_token(2)}\'',
                f'var {m_name} = new COBOLVar("{gen_mask_token(2)}", 2);'
            ),
            (
                f'{m_level} {m_name} PIC XXX VALUE \'{gen_mask_token(2)}\'',
                f'var {m_name} = new COBOLVar("{gen_mask_token(2)}", 3);'
            ),
            (
                f'{m_level} {m_name} PIC XXXX VALUE \'{gen_mask_token(2)}\'',
                f'var {m_name} = new COBOLVar("{gen_mask_token(2)}", 4);'
            ),
            (
                f'{m_level} {m_name} PIC X VALUE " "',
                f'var {m_name} = new COBOLVar("{gen_mask_token(2)}", 1);'
            ),
            (
                f'{m_level} {m_name} PIC X VALUE "{gen_mask_token(2)}"',
                f'var {m_name} = new COBOLVar("{gen_mask_token(2)}", 1);'
            ),
            (
                f'{m_level} {m_name} PIC XX VALUE "{gen_mask_token(2)}"',
                f'var {m_name} = new COBOLVar("{gen_mask_token(2)}", 2);'
            ),
            (
                f'{m_level} {m_name} PIC XXX VALUE "{gen_mask_token(2)}"',
                f'var {m_name} = new COBOLVar("{gen_mask_token(2)}", 3);'
            ),
            (
                f'{m_level} {m_name} PIC XXXX VALUE "{gen_mask_token(2)}"',
                f'var {m_name} = new COBOLVar("{gen_mask_token(2)}", 4);'
            ),
            (
                f'{m_level} {m_name} PIC X({m_size}) VALUE \'{gen_mask_token(3)}\'',
                f'var {m_name} = new COBOLVar("{gen_mask_token(3)}", {m_size});'
            ),
            (
                f'{m_level} {m_name} PIC X({m_size}) VALUE "{gen_mask_token(3)}"',
                f'var {m_name} = new COBOLVar("{gen_mask_token(3)}", {m_size});'
            ),
            (
                f'{m_level} {m_name} PIC X({m_size}) VALUE \' \'',
                f'var {m_name} = new COBOLVar(new string(\' \', {m_size}), {m_size});'
            ),
            (
                f'{m_level} {m_name} PIC X({m_size}) VALUE " "',
                f'var {m_name} = new COBOLVar(new string(\' \', {m_size}), {m_size});'
            ),
            (
                f'{m_level} {m_name} PIC X({m_size}) VALUE SPACE',
                f'var {m_name} = new COBOLVar(new string(\' \', {m_size}), {m_size});'
            ),
            (
                f'{m_level} {m_name} PIC X({m_size}) VALUE SPACES',
                f'var {m_name} = new COBOLVar(new string(\' \', {m_size}), {m_size});'
            ),
            (
                f'{m_level} {m_name} PIC X({m_size}) VALUE NULL',
                f'var {m_name} = new COBOLVar(null, {m_size});'
            ),
            # Alphabetic
            (
                f'{m_level} {m_name} PIC A VALUE \' \'',
                f'var {m_name} = new COBOLVar("{gen_mask_token(2)}", 1);'
            ),
            (
                f'{m_level} {m_name} PIC A VALUE \'{gen_mask_token(2)}\'',
                f'var {m_name} = new COBOLVar("{gen_mask_token(2)}", 1);'
            ),
            (
                f'{m_level} {m_name} PIC AA VALUE \'{gen_mask_token(2)}\'',
                f'var {m_name} = new COBOLVar("{gen_mask_token(2)}", 2);'
            ),
            (
                f'{m_level} {m_name} PIC AAA VALUE \'{gen_mask_token(2)}\'',
                f'var {m_name} = new COBOLVar("{gen_mask_token(2)}", 3);'
            ),
            (
                f'{m_level} {m_name} PIC AAAA VALUE \'{gen_mask_token(2)}\'',
                f'var {m_name} = new COBOLVar("{gen_mask_token(2)}", 4);'
            ),
            (
                f'{m_level} {m_name} PIC A VALUE " "',
                f'var {m_name} = new COBOLVar("{gen_mask_token(2)}", 1);'
            ),
            (
                f'{m_level} {m_name} PIC A VALUE "{gen_mask_token(2)}"',
                f'var {m_name} = new COBOLVar("{gen_mask_token(2)}", 1);'
            ),
            (
                f'{m_level} {m_name} PIC AA VALUE "{gen_mask_token(2)}"',
                f'var {m_name} = new COBOLVar("{gen_mask_token(2)}", 2);'
            ),
            (
                f'{m_level} {m_name} PIC AAA VALUE "{gen_mask_token(2)}"',
                f'var {m_name} = new COBOLVar("{gen_mask_token(2)}", 3);'
            ),
            (
                f'{m_level} {m_name} PIC AAAA VALUE "{gen_mask_token(2)}"',
                f'var {m_name} = new COBOLVar("{gen_mask_token(2)}", 4);'
            ),
            (
                f'{m_level} {m_name} PIC A({m_size}) VALUE \'{gen_mask_token(3)}\'',
                f'var {m_name} = new COBOLVar("{gen_mask_token(3)}", {m_size});'
            ),
            (
                f'{m_level} {m_name} PIC A({m_size}) VALUE "{gen_mask_token(3)}"',
                f'var {m_name} = new COBOLVar("{gen_mask_token(3)}", {m_size});'
            ),
            (
                f'{m_level} {m_name} PIC A({m_size}) VALUE \' \'',
                f'var {m_name} = new COBOLVar(new string(\' \', {m_size}), {m_size});'
            ),
            (
                f'{m_level} {m_name} PIC A({m_size}) VALUE " "',
                f'var {m_name} = new COBOLVar(new string(\' \', {m_size}), {m_size});'
            ),
            (
                f'{m_level} {m_name} PIC A({m_size}) VALUE SPACE',
                f'var {m_name} = new COBOLVar(new string(\' \', {m_size}), {m_size});'
            ),
            (
                f'{m_level} {m_name} PIC A({m_size}) VALUE SPACES',
                f'var {m_name} = new COBOLVar(new string(\' \', {m_size}), {m_size});'
            ),
            (
                f'{m_level} {m_name} PIC A({m_size}) VALUE NULL',
                f'var {m_name} = new COBOLVar(null, {m_size});'
            ),
            # Integers
            (
                f'{m_level} {m_name} PIC 9 VALUE {gen_mask_token(2)}',
                f'var {m_name} = new COBOLVar({gen_mask_token(2)}, 1);'
            ),
            (
                f'{m_level} {m_name} PIC 99 VALUE {gen_mask_token(2)}',
                f'var {m_name} = new COBOLVar({gen_mask_token(2)}, 2);'
            ),
            (
                f'{m_level} {m_name} PIC 999 VALUE {gen_mask_token(2)}',
                f'var {m_name} = new COBOLVar({gen_mask_token(2)}, 3);'
            ),
            (
                f'{m_level} {m_name} PIC 9999 VALUE {gen_mask_token(2)}',
                f'var {m_name} = new COBOLVar({gen_mask_token(2)}, 4);'
            ),
            (
                f'{m_level} {m_name} PIC 9({m_size}) VALUE {gen_mask_token(3)}',
                f'var {m_name} = new COBOLVar({gen_mask_token(3)}, {m_size});'
            ),
            (
                f'{m_level} {m_name} PIC 9({m_size}) VALUE NULL',
                f'var {m_name} = new COBOLVar(null, {m_size});'
            ),
            # Integers with literal 0 default value
            (
                f'{m_level} {m_name} PIC 9({m_size}) VALUE ZERO',
                f'var {m_name} = new COBOLVar(0, {m_size});'
            ),
            (
                f'{m_level} {m_name} PIC 9({m_size}) VALUE ZEROS',
                f'var {m_name} = new COBOLVar(0, {m_size});'
            ),
            (
                f'{m_level} {m_name} PIC 9({m_size}) VALUE ZEROES',
                f'var {m_name} = new COBOLVar(0, {m_size});'
            ),
            # Signed integers
            (
                f'{m_level} {m_name} PIC S9 VALUE {gen_mask_token(2)}',
                f'var {m_name} = new COBOLVar({gen_mask_token(2)}, 1);'
            ),
            (
                f'{m_level} {m_name} PIC S99 VALUE {gen_mask_token(2)}',
                f'var {m_name} = new COBOLVar({gen_mask_token(2)}, 2);'
            ),
            (
                f'{m_level} {m_name} PIC S999 VALUE {gen_mask_token(2)}',
                f'var {m_name} = new COBOLVar({gen_mask_token(2)}, 3);'
            ),
            (
                f'{m_level} {m_name} PIC S9999 VALUE {gen_mask_token(2)}',
                f'var {m_name} = new COBOLVar({gen_mask_token(2)}, 4);'
            ),
            (
                f'{m_level} {m_name} PIC S9({m_size}) VALUE {gen_mask_token(3)}',
                f'var {m_name} = new COBOLVar({gen_mask_token(3)}, {m_size});'
            ),
            (
                f'{m_level} {m_name} PIC S9({m_size}) VALUE NULL',
                f'var {m_name} = new COBOLVar(null, {m_size});'
            ),
            # Floats
            (
                f'{m_level} {m_name} PIC S9({m_size})V9({gen_mask_token(3)}) VALUE {gen_mask_token(4)}',
                # NOTE: C# requires "f" suffix for float literals
                f'var {m_name} = new COBOLVar({gen_mask_token(4)}f, {m_size} + {gen_mask_token(3)} + 1);'
            ),
            (
                f'{m_level} {m_name} PIC S9({m_size})V9({gen_mask_token(3)}) VALUE NULL',
                # NOTE: C# requires "f" suffix for float literals
                f'var {m_name} = new COBOLVar(null, {m_size} + {gen_mask_token(3)} + 1);'
            ),
            # Signed integers with literal 0 default value
            (
                f'{m_level} {m_name} PIC S9({m_size}) VALUE ZERO',
                f'var {m_name} = new COBOLVar(0, {m_size});'
            ),
            (
                f'{m_level} {m_name} PIC S9({m_size}) VALUE ZEROS',
                f'var {m_name} = new COBOLVar(0, {m_size});'
            ),
            (
                f'{m_level} {m_name} PIC S9({m_size}) VALUE ZEROES',
                f'var {m_name} = new COBOLVar(0, {m_size});'
            ),
            # Floats with literal 0 default value
            (
                f'{m_level} {m_name} PIC S9({m_size})V9({gen_mask_token(3)}) VALUE ZERO',
                f'var {m_name} = new COBOLVar(0f, {m_size} + {gen_mask_token(3)} + 1);'
            ),
            (
                f'{m_level} {m_name} PIC S9({m_size})V9({gen_mask_token(3)}) VALUE ZEROS',
                f'var {m_name} = new COBOLVar(0f, {m_size} + {gen_mask_token(3)} + 1);'
            ),
            (
                f'{m_level} {m_name} PIC S9({m_size})V9({gen_mask_token(3)}) VALUE ZEROES',
                f'var {m_name} = new COBOLVar(0f, {m_size} + {gen_mask_token(3)} + 1);'
            ),
        ]

        # Add "FILLER" syntax pairs
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
                pairs_with_values
            )
        )

        pairs.extend(pairs_with_values)
        pairs.extend(filler_pairs)

        # Convert pairs to writable items
        items = map(lambda p: gen_item(p[0], p[1]), pairs)

        # Write items to output
        for i in items:
            write(i)
