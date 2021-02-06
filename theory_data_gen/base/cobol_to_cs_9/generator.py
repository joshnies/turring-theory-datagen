from .arithmetic import gen_arithmetic
from .conditionals import gen_conditionals
from .loops import gen_loops
from .stdout import gen_stdout
from .vars import gen_vars
from ...generator import Generator


class CobolToCS9Generator(Generator):
    """Data generator for COBOL to C# 9."""

    @staticmethod
    def generate(args, write):
        print('\nGenerating dataset for COBOL --> C# 9')

        gen_vars(write)
        gen_arithmetic(write, args.arithmetic)
        gen_conditionals(write, args.conditionals)
        gen_loops(write, args.loops)
        gen_stdout(write)

        # TODO: Implement COBOL "EVALUATE" statements to C# switch statements
        # TODO: Implement COBOL subroutines to C# function calls
        # TODO: Implement COBOL reports
        # TODO: Implement COBOL FILLER items
        # TODO: Implement built-in function calls
