from theory_data_gen.common import gen_item, gen_mask_token
from theory_data_gen.generator import Generator


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

        items = [
            gen_item(
                f'{gen_mask_token(0)} {gen_mask_token(1)} OCCURS {gen_mask_token(2)} TIMES PIC XX',
                f'var {gen_mask_token(1)} = new COBOLVar(new string("", 2), size: 2, occurs: {gen_mask_token(2)});'
            ),
        ]

        for i in items:
            write(i)
