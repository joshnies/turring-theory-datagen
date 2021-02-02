from theory_data_gen.generator import Generator
from .core import gen_core
from .stdout import gen_stdout
from .vars import gen_vars


class IBMCobolToCS7Generator(Generator):
    """Data generator for IBM COBOL to C# 7."""

    @staticmethod
    def generate(args, write):
        print('\nGenerating dataset for IBM COBOL --> C# 7')

        gen_core(write)
        gen_vars(write)
        gen_stdout(write)

        # TODO: Implement remaining data
