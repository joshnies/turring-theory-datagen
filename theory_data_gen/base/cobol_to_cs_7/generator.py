from theory_data_gen.generator import Generator
from .arithmetic import gen_arithmetic
from .conditionals import gen_conditionals
from .stdout import gen_stdout
from .vars import gen_vars


class CobolToCS7Generator(Generator):
    """Data generator for COBOL to C# 7."""

    @staticmethod
    def generate(args, write):
        print('\nGenerating dataset for COBOL --> C# 7')

        gen_vars(write)
        gen_arithmetic(write, args.arithmetic)
        gen_conditionals(write, args.conditionals)
        gen_stdout(write)

        # TODO: Implement remaining data
