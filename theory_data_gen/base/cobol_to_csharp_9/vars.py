from yaspin import yaspin

from ...constants import MASK_TOKEN
from .common import gen_filler_pairs
from ...common import gen_item, gen_mask_token, add_mask_indices

COBOL_STR_TYPES = ['X', 'A']
COBOL_INT_TYPES = ['9', 'S9']
QUOTES = ['"', "'"]


def __gen_default_val(src_type: str, size: int):
    """
    Generate target default value.

    :param src_type: Source data type.
    :param size: Source variable size.
    :returns: Default value.
    """

    # Strings
    if src_type in COBOL_STR_TYPES:
        if size == 1:
            return '" "'

        return f"new string(' ', {size})"

    # Integers
    if src_type in COBOL_INT_TYPES:
        return '0'

    return 'null'


def __gen_vars():
    """Generate variable definitions."""

    # Generate static mask tokens
    m_level = gen_mask_token(0)
    m_name = gen_mask_token(1)
    m_occurrence = gen_mask_token(2)
    m_indexed_by = gen_mask_token(3)

    pairs = list()
    types = ['X', 'A', '9']

    # Generate variables
    for t in types:
        for i in range(1, 21):
            for i_is_signed in range(2):
                for i_use_occurs in range(2):
                    for i_use_indexed in range(2):
                        for i_use_z in range(2):
                            is_signed = bool(i_is_signed)
                            use_occurs = bool(i_use_occurs)
                            use_indexed = use_occurs and bool(i_use_indexed)
                            use_z = bool(i_use_z)

                            # Offset "size" mask token based on previous clauses in source
                            m_size_index = 2 + int(use_occurs) + int(use_indexed)
                            m_size = gen_mask_token(m_size_index)

                            prefix = 'S' if is_signed and t == '9' else ''

                            if use_z:
                                z_range = 'Z' * (i - 1)
                                src_type = z_range + prefix + t
                            else:
                                type_range = t * i
                                src_type = prefix + type_range

                            src_occurs = f' OCCURS {m_occurrence} TIMES ' if use_occurs else ' '
                            src_indexed = f'INDEXED BY {m_indexed_by} ' if use_indexed else ''

                            tar_occurs = f', occurs: {m_occurrence}' if use_occurs else ''
                            tar_indexed = fr'\n{m_indexed_by} = new COBOLVar(0, size: 10);' if use_indexed else ''

                            for combined_type_idx, combined_type in enumerate([src_type, f'{prefix}{t}({m_size})']):
                                tar_size = i if combined_type_idx == 0 else m_size
                                m_val = gen_mask_token(m_size_index + combined_type_idx)

                                # No default value
                                pairs.append((
                                    f'{m_level} {m_name}{src_occurs}{src_indexed}PIC {combined_type}',
                                    f'{m_name} = new COBOLVar({__gen_default_val(t, tar_size)}, size: {tar_size}{tar_occurs});{tar_indexed}',
                                ))

                                # Null default value
                                pairs.append((
                                    f'{m_level} {m_name}{src_occurs}{src_indexed}PIC {combined_type} VALUE NULL',
                                    f'{m_name} = new COBOLVar(null, size: {tar_size}{tar_occurs});{tar_indexed}',
                                ))

                                # Masked default value
                                pairs.append((
                                    f'{m_level} {m_name}{src_occurs}{src_indexed}PIC {combined_type} VALUE {m_val}',
                                    f'{m_name} = new COBOLVar({m_val}, size: {tar_size}{tar_occurs});{tar_indexed}',
                                ))

                                for quote in QUOTES:
                                    pairs.append((
                                        f'{m_level} {m_name}{src_occurs}{src_indexed}PIC {combined_type} VALUE {quote}{m_val}{quote}',
                                        f'{m_name} = new COBOLVar("{m_val}", size: {tar_size}{tar_occurs});{tar_indexed}',
                                    ))

                                # String-specific default values
                                if t in COBOL_STR_TYPES:
                                    for val in ['SPACE', 'SPACES']:
                                        pairs.append((
                                            f'{m_level} {m_name}{src_occurs}{src_indexed}PIC {combined_type} VALUE {val}',
                                            f"{m_name} = new COBOLVar(new string(' ', {tar_size}), size: {tar_size}{tar_occurs});{tar_indexed}",
                                        ))

                                    for quote in QUOTES:
                                        pairs.append((
                                            f'{m_level} {m_name}{src_occurs}{src_indexed}PIC {combined_type} VALUE {quote * 2}',
                                            f'{m_name} = new COBOLVar("", size: {tar_size}{tar_occurs});{tar_indexed}',
                                        ))

                                        pairs.append((
                                            f'{m_level} {m_name}{src_occurs}{src_indexed}PIC {combined_type} VALUE {quote} {quote}',
                                            f'{m_name} = new COBOLVar(" ", size: {tar_size}{tar_occurs});{tar_indexed}',
                                        ))

                                    # "HIGH/LOW-VALUES" default values
                                    pairs.append((
                                        f'{m_level} {m_name}{src_occurs}{src_indexed}PIC {combined_type} VALUE HIGH-VALUES',
                                        f"{m_name} = new COBOLVar(new string('.', {tar_size}), size: {tar_size}{tar_occurs});{tar_indexed}",
                                    ))
                                    pairs.append((
                                        f'{m_level} {m_name}{src_occurs}{src_indexed}PIC {combined_type} VALUE LOW-VALUES',
                                        f'{m_name} = new COBOLVar(string.Concat(Enumerable.Repeat("<NUL>", {tar_size})), size: {tar_size}{tar_occurs});{tar_indexed}',
                                    ))

                                # Generate decimal type vars
                                if t == '9':
                                    for z_count in range(0, 6):
                                        z = 'Z' * z_count
                                        pairs.append((
                                            f'{m_level} {m_name}{src_occurs}{src_indexed}PIC {z}9',
                                            f'{m_name} = new COBOLVar(0f, size: {z_count + 1}{tar_occurs});{tar_indexed}'
                                        ))

                                        for dec_count in range(1, 6):
                                            dec_nines = '9' * dec_count
                                            pairs.append((
                                                f'{m_level} {m_name}{src_occurs}{src_indexed}PIC {z}9.{dec_nines}',
                                                f'{m_name} = new COBOLVar(0f, size: {z_count + dec_count + 1}{tar_occurs});{tar_indexed}'
                                            ))

                                            dec_z = 'Z' * (dec_count - 1)
                                            pairs.append((
                                                f'{m_level} {m_name}{src_occurs}{src_indexed}PIC {z}9.{dec_z}9',
                                                f'{m_name} = new COBOLVar(0f, size: {z_count + dec_count + 1}{tar_occurs});{tar_indexed}'
                                            ))

                                # Zero default value
                                tar_val = f"new string('0', {tar_size})" if t in COBOL_STR_TYPES else '0'
                                for val in ['ZERO', 'ZEROS', 'ZEROES']:
                                    pairs.append((
                                        f'{m_level} {m_name}{src_occurs}{src_indexed}PIC {combined_type} VALUE {val}',
                                        f'{m_name} = new COBOLVar({tar_val}, size: {tar_size}{tar_occurs});{tar_indexed}',
                                    ))

    return pairs


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
                f'{m_name} = new COBOLVar(0f, 3);'
            ),
            (
                f'{m_level} {m_name} PIC 9({m_size})V9({gen_mask_token(3)})',
                # NOTE: Literal operations are calculated at compile-time in C#
                f'{m_name} = new COBOLVar(0f, size: {m_size} + {gen_mask_token(3)} + 1);'
            ),
            # Signed floats
            (
                f'{m_level} {m_name} PIC S9V9',
                f'{m_name} = new COBOLVar(0f, size: 3);'
            ),
            (
                f'{m_level} {m_name} PIC S9({m_size})V9({gen_mask_token(3)})',
                # NOTE: Literal operations are calculated at compile-time in C#
                f'{m_name} = new COBOLVar(0f, size: {m_size} + {gen_mask_token(3)} + 1);'
            ),
            # Floats with default value
            (
                f'{m_level} {m_name} PIC S9({m_size})V9({gen_mask_token(3)}) VALUE {gen_mask_token(4)}',
                # NOTE: C# requires "f" suffix for float literals
                f'{m_name} = new COBOLVar({gen_mask_token(4)}f, size: {m_size} + {gen_mask_token(3)} + 1);'
            ),
            (
                f'{m_level} {m_name} PIC S9({m_size})V9({gen_mask_token(3)}) VALUE NULL',
                # NOTE: C# requires "f" suffix for float literals
                f'{m_name} = new COBOLVar(null, size: {m_size} + {gen_mask_token(3)} + 1);'
            ),
            # Floats with literal 0 default value
            (
                f'{m_level} {m_name} PIC S9({m_size})V9({gen_mask_token(3)}) VALUE ZERO',
                f'{m_name} = new COBOLVar(0f, size: {m_size} + {gen_mask_token(3)} + 1);'
            ),
            (
                f'{m_level} {m_name} PIC S9({m_size})V9({gen_mask_token(3)}) VALUE ZEROS',
                f'{m_name} = new COBOLVar(0f, size: {m_size} + {gen_mask_token(3)} + 1);'
            ),
            (
                f'{m_level} {m_name} PIC S9({m_size})V9({gen_mask_token(3)}) VALUE ZEROES',
                f'{m_name} = new COBOLVar(0f, size: {m_size} + {gen_mask_token(3)} + 1);'
            ),
        ])

        # Add "FILLER" syntax pairs
        pairs = gen_filler_pairs(pairs)

        # Convert pairs to writable items
        items = map(lambda p: gen_item(p[0], p[1]), pairs)

        # Write items to output
        for i in items:
            write(i)
