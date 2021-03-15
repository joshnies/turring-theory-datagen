from theory_data_gen.common import gen_item, gen_mask_token
from theory_data_gen.generator import Generator


# TODO: Remove if no longer needed
class FGREGGTaxExtensionToCSharp9Generator(Generator):
    """
    Case generator for "fgregg/tax_extension":
    https://github.com/fgregg/tax_extension

    Source: COBOL
    Target: C# 9
    """

    @staticmethod
    def generate(args, write):
        print('\nGenerating case data for "fgregg/tax_extension" --> C# 9')

        items = list()

        for i in items:
            write(i)
