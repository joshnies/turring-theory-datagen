from .arithmetic import gen_arithmetic
from .conditionals import gen_conditionals
from .loops import gen_loops
from .redefines import gen_redefines
from .stdout import gen_stdout
from .vars import gen_vars
from ...generator import Generator


class CobolToCSharp9Generator(Generator):
    """Data generator for COBOL to C# 9."""

    @staticmethod
    def generate_datasets(args, write):
        print('\nGenerating datasets for COBOL --> C# 9')

        gen_arithmetic(write, args.arithmetic)
        gen_conditionals(write, args.conditionals)
        gen_loops(write, args.loops)

    @staticmethod
    def generate_map_data(args, write):
        print('\nGenerating data map for COBOL --> C# 9')

        gen_vars(write)
        gen_redefines(write)
        gen_stdout(write)
