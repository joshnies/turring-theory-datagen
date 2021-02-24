from yaspin import yaspin

from constants import MASK_TOKEN
from ...common import gen_item, gen_mask_token, add_mask_indices

TYPE_DEF_VAL_MAP = {
    'X': '""',
    'A': '""',
    '9': '0',
    'S9': '0',
}

STR_TYPES = ['X', 'A']


def __gen_vars():
    """Generate variable definitions."""

    pairs = list()
    types = TYPE_DEF_VAL_MAP.keys()

    # Generate variables
    for t in types:
        for i in range(1, 100):
            pairs.append((
                f'{MASK_TOKEN} {MASK_TOKEN} PIC {t * i}',
                f'var {MASK_TOKEN} = new COBOLVar({TYPE_DEF_VAL_MAP[t]}, {i});',
            ))

            pairs.append((
                f'{MASK_TOKEN} {MASK_TOKEN} PIC {t}({i})',
                f'var {MASK_TOKEN} = new COBOLVar({TYPE_DEF_VAL_MAP[t]}, {i});',
            ))

            # Default values
            pairs.append((
                f'{MASK_TOKEN} {MASK_TOKEN} PIC {t * i} VALUE NULL',
                f'var {MASK_TOKEN} = new COBOLVar(null, {i});',
            ))

            pairs.append((
                f'{MASK_TOKEN} {MASK_TOKEN} PIC {t * i} VALUE {MASK_TOKEN}',
                f'var {MASK_TOKEN} = new COBOLVar({MASK_TOKEN}, {i});',
            ))

            pairs.append((
                f'{MASK_TOKEN} {MASK_TOKEN} PIC {t * i} VALUE "{MASK_TOKEN}"',
                f'var {MASK_TOKEN} = new COBOLVar("{MASK_TOKEN}", {i});',
            ))

            pairs.append((
                f"{MASK_TOKEN} {MASK_TOKEN} PIC {t * i} VALUE '{MASK_TOKEN}'",
                f'var {MASK_TOKEN} = new COBOLVar("{MASK_TOKEN}", {i});',
            ))

            # String-specific default values
            if t in STR_TYPES:
                for val in ['SPACE', 'SPACES']:
                    pairs.append((
                        f'{MASK_TOKEN} {MASK_TOKEN} PIC {t * i} VALUE {val}',
                        f'var {MASK_TOKEN} = new COBOLVar(new string(" ", {i}), {i});',
                    ))

                for quote in ['"', "'"]:
                    pairs.append((
                        f'{MASK_TOKEN} {MASK_TOKEN} PIC {t * i} VALUE {quote * 2}',
                        f'var {MASK_TOKEN} = new COBOLVar("", {i});',
                    ))

                    pairs.append((
                        f'{MASK_TOKEN} {MASK_TOKEN} PIC {t * i} VALUE {quote} {quote}',
                        f'var {MASK_TOKEN} = new COBOLVar(" ", {i});',
                    ))
            # Numeric-specific default values
            else:
                tar_val = TYPE_DEF_VAL_MAP[t]
                for val in ['ZERO', 'ZEROS', 'ZEROES']:
                    pairs.append((
                        f'{MASK_TOKEN} {MASK_TOKEN} PIC {t * i} VALUE {val}',
                        f'var {MASK_TOKEN} = new COBOLVar({tar_val}, {i});',
                    ))

    # Add mask indices
    items = list()
    for src, tar in pairs:
        new_src, _ = add_mask_indices(src)
        new_tar, _ = add_mask_indices(tar, start_index=1)
        items.append((new_src, new_tar))

    return items


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

        # Generate pairs
        pairs = __gen_vars()
        pairs.extend([
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
            # Floats with default value
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
        ])

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
                pairs
            )
        )

        pairs.extend(filler_pairs)

        # Convert pairs to writable items
        items = map(lambda p: gen_item(p[0], p[1]), pairs)

        # Write items to output
        for i in items:
            write(i)
